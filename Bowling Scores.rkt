#lang racket
(require rackunit)

(define (read-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((result '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse result)
              (let ((tokens (string-split line)))
                (loop (cons tokens result)))))))))

; Read scores.txt into lines
(define lines (read-lines "scores.txt"))

;Create list of teams
(define teams (list (list-ref lines 0) (list-ref lines 16)))


;return first two elements for a name
(define (first-two-elements lst)
  (take lst 2))

;remove name for score counting
(define (remove-first-two-elements lst)
  (drop lst 2))

(define (convert-string-to-symbol-or-number item)
  (cond
    [(list? item) ; If the item is a list, recursively process each element
     (map convert-string-to-symbol-or-number item)]
    [(string? item) ; If the item is a string, determine if it should be a number or symbol
     (let ((maybe-number (string->number item)))
       (if maybe-number
           maybe-number ; If the string is a valid number, convert it
           (string->symbol item)))] ; Otherwise, convert it to a symbol
    [else
     item])) ; If it's not a list or string, leave it unchanged


(define (group-into-frames rolls)
  (let loop ([rolls rolls] [frames '()] [current-frame '()] [frame-count 0])
    (cond 
      [(or (null? rolls) (= frame-count 10)) (reverse (cons current-frame frames))]
      [else
       (let ([roll (car rolls)])
         (match roll
           ;; Strike: Add a new frame with just 'X', unless it's the 10th frame
           ['X (if (< frame-count 9)
                   (loop (cdr rolls) (cons (list 'X) frames) '() (+ frame-count 1))
                   (loop (cdr rolls) frames (cons 'X current-frame) frame-count))]
           ;; Spare: Complete the current frame with '/', start a new frame
           ['/ (loop (cdr rolls) (cons (append current-frame (list '/)) frames) '() (+ frame-count 1))]
           ;; Regular roll: Add roll to the current frame. If it's the second roll in the frame (or a spare), start a new frame.
           [n (if (or (= (length current-frame) 1) (and (= (length current-frame) 2) (equal? (second current-frame) 'X)))
                  (loop (cdr rolls) (cons (append current-frame (list n)) frames) '() (+ frame-count 1))
                  (loop (cdr rolls) frames (cons n current-frame) frame-count))]))])))

;Turn list into a string
(define (list->string lst)
  (string-join (map ~a lst) ""))

;Turn list into a string with spaceing
(define (list->string-with-space lst)
  (string-join (map ~a lst) " "))

(define team-1 (list->string(list-ref lines 0)))
(define team-2 (list->string(list-ref lines 16)))

;Turn string into a list
(define (parse-input input)
  (map string->list
       (string-split input)))

(define (frame->score frame)
  (define (loop remain tally)
    (if (empty? remain) tally
        (cond
          [(char=? (first remain) #\-)
           (loop (rest remain) tally)]
          [(char=? (first remain) #\X)
           (loop (rest remain) (+ tally 10))]
          [(char=? (first remain) #\/)
           (loop (rest remain) 10)]
          [else
           (loop (rest remain)
                 (+ tally
                    (- (char->integer (first remain)) 48)))]
          )))
  (loop frame 0))

;Add bouns frames if needed
(define (add-bonus-frames frames tally frame-type)
  (case frame-type
    ['strike
     (+ tally 10
        (frame->score (take (flatten (rest frames)) 2)))]
    ['spare
     (+ tally 10
        (frame->score (take (flatten (rest frames)) 1)))]
    ))
;score the game
(define (tally-game frames (tally 0))
  (if (= (length frames) 1)
      (+ tally
         (frame->score (first frames)))
      (tally-game
       (rest frames)
       (cond
         [(member #\X (first frames))
          (add-bonus-frames frames tally 'strike)]
         [(member #\/ (first frames))
          (add-bonus-frames frames tally 'spare)]
         [else
          (+ tally (frame->score (first frames)))])))) 

(define(calculate-score line)
   (tally-game (parse-input (list->string-with-space (map list->string (group-into-frames (convert-string-to-symbol-or-number (remove-first-two-elements line))))))))

;Remove any duplicates from a list
(define (remove-duplicates lst)
  (foldl (lambda (item acc) ; item is the current element, acc is the accumulator (new list)
           (if (member item acc) ; Check if item is already in the accumulator
               acc              ; If yes, return the accumulator unchanged
               (cons item acc))) ; If no, add item to the accumulator
         '() 
         lst))

;Remove teams from the list
(define (remove-specific-indices lines)
  (define players
    (remove* (list (list-ref lines 0)
                   (list-ref lines 16))
             lines))
  players)

;Creates a list with the teams removed
(define score-lines(remove-specific-indices lines))

;Create a list of the players
(define players (list->string-with-space (remove-duplicates (map first-two-elements (remove-specific-indices lines)))))

;Tag Player and Team to Scores
(define (tag-scores-with-names-and-teams lines)
  (define (team-assignment index)
    (if (< index 15) (list-ref teams 0) (list-ref teams 1))) ; Determine the team based on index

  (map-indexed (lambda (index line)
                 (let ((name (list->string-with-space(first-two-elements line))) ; Extract name
                       (score (calculate-score line)) ; Calculate score
                       (team (team-assignment index))) ; Determine team
                   (list name score (list->string team)))) ; Create the tagged list
               lines))

(define (map-indexed f lst)
  (let loop ((i 0) (lst lst) (acc '()))
    (if (null? lst)
        (reverse acc)
        (loop (+ i 1) (cdr lst) (cons (f i (car lst)) acc)))))

(define tagged-scores (tag-scores-with-names-and-teams (remove-specific-indices lines)))

;Aggregate scores by player using immutable hash operations
(define (aggregate-scores-by-player tagged-scores)
  (let loop ((remaining tagged-scores) (scores-hash (hash)))
    (if (null? remaining)
        scores-hash
        (let* ((entry (first remaining))
               (name (first entry))
               (score (second entry))
               (updated-scores (cons score (hash-ref scores-hash name '()))))
          (loop (rest remaining) (hash-set scores-hash name updated-scores))))))

;Display scores and totals for each player
(define (display-scores-and-totals aggregated-scores-hash)
  (hash-for-each aggregated-scores-hash
                 (lambda (name scores)
                   (let ((total (apply + scores)))
                     (printf "~a: Scores = ~a, Total = ~a\n" name scores total)))))

;Filter scores by team name
(define (filter-scores-by-team tagged-scores team-name)
  (filter (lambda (score-entry)
            (string=? (third score-entry) team-name)) ; Check if the team name matches
          tagged-scores))


;Filter scores by team
(define team-1-totals (filter-scores-by-team tagged-scores team-1))
(define team-2-totals (filter-scores-by-team tagged-scores team-2))


(define team-1-scores (aggregate-scores-by-player team-1-totals))
(define team-2-scores (aggregate-scores-by-player team-2-totals))

;Sum all scores in a list
(define (sum-scores scores-list)
  (foldl (lambda (score acc)
           (+ score acc))
         0
         scores-list))

(define (sum-tagged-scores tagged-scores)
  (foldl (lambda (entry acc)
           (+ (second entry) acc))
         0
         tagged-scores))

(define team-1-score(sum-tagged-scores team-1-totals))
(define team-2-score(sum-tagged-scores team-2-totals))

;Find and Display Winning team
(define (winning-team team-1-score team-2-score)
  (displayln "Winners:")
  (if (> team-1-score team-2-score)
  (displayln team-1)
  (displayln team-2))  
  )

;Find the highest scoring player
(define (player-with-highest-score players-scores)
  (foldl (lambda (current-player highest)
           (if (> (second current-player) (second highest))
               current-player
               highest))
         (first players-scores)
         (rest players-scores)))

(define highest-scorer (player-with-highest-score tagged-scores))

;Output
(display "Team 1: ")
(displayln team-1)
(display-scores-and-totals team-1-scores)
(display "Team 1 Total Score : ")
(displayln team-1-score)
(displayln "-------------")
(display "Team 2: ")
(displayln team-2)
(display-scores-and-totals team-2-scores)
(display "Team 2 Total Score : ")
(displayln team-2-score)
(displayln "-------------")
(winning-team team-1-score team-2-score)
(displayln "-------------")
(displayln "Most Valuable Player:")
(displayln (first highest-scorer))