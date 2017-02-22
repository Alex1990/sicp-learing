; The result printed by the interpreter
; (1 (2 (3 4)))
(display (list 1 (list 2 (list 3 4))))
(newline)

; The box-and-pointer structure
;
; (1 (2 (3 4)))     (2 (3 4)         (3 4)
;      ↓               ↓               ↓
;  ┌───┬───┐       ┌───┬───┐       ┌───┬───┐       ┌───┬───┐
;  │ • │ • │  ⟶    │ • │ • │  ⟶    │ • │ • │  ⟶    │ • │ ⁄ │
;  └───┴───┘       └───┴───┘       └───┴───┘       └───┴───┘
;    ↓               ↓               ↓               ↓
;    1               2               3               4

; Tree representation
;
; (1 (2 (3 4)))
;   ---•---
;  /       \ (2 (3 4))
; 1     ---•---
;      /       \ (3 4)
;     2      ---•---
;           /       \
;          3         4
