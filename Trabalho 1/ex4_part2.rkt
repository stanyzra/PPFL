#lang racket
(require examples)

#|
Uma string é palíndromo se as letras da string na ordem normal ou de trás para frente são as mesmas (ignorando
diferenças entre minúsculas e maiúsculas e sinais diacríticos). Projete uma função que verifique se uma string
sem os caracteres de pontuação é palíndromo. Assuma que a string seja composta apenas de letras do alfabeto
(modificadas ou não com sinais diacríticos), números e símbolos usados na Língua Portuguesa. Você pode usar
a função (string-split s "") para gerar uma lista com os elementos da string s e processar esses elementos,
dessa forma você não precisa trabalhar com caracteres (que não vimos em sala), apenas com strings.
|#

#| Análise:
Verificar se uma string é palíndromo, ou seja, se o início até a metade e a metade até o fim da string são
iguais. Essa string pode contém letras do alfabeto, números e símbolos usados na Língua Portuguesa. Pode-se
adotar a estratégia de verificar se a posição início + 1 até o fim - 1 é palíndromo, recursivamente, e por
fim, verificar se o elemento da primeira posição é igual a última.
|#

; ##############################################################################################

#| Definição dos tipos de dados:

letras-minusculas (lower-letters) é uma lista contendo todas as letras minúsculas do alfabeto da
Língua Portuguesa.

|#

; ##############################################################################################

#| Especificação:
string list int -> boolean
Recebe uma string, uma lista contendo o alfabeto mínusculo e um controlador. Verifica se a string
passada é um símbolo ou um espaço. Se for, o controlador é incrementado em 1, caso contrário. Ao final
retorna true caso o controlador for maior que 1, significando a str passada por parâmetro está contida
na lista de alfabeto.
|#

(define (is-alphabet-element? str alphabet number-matches)
  (cond
    [(empty? alphabet) (> number-matches 0)] 
    [(string=? (first alphabet) str) (is-alphabet-element? str (rest alphabet) (+ number-matches 1))]
    [else
     (is-alphabet-element? str (rest alphabet) number-matches)]))

#| Especificação:
list -> list

Recebe uma lista de string e remove os espaços e símbolos especiais
|#

(define (normalize-string-list string-list)
  (cond
    [(empty? string-list) empty]
    [else
     (if (is-alphabet-element? (first string-list) lower-letters-and-numbers 0)
         (cons (first string-list) (normalize-string-list (rest string-list)))
         (normalize-string-list (rest string-list))
         )]))

#| Especificação:
string -> string

Recebe uma string de uma letra do alfabeto e remove o caracter especial dela.
|#

(define (remove-special-character-string str)
  (cond
    [(or (string=? str "á")
         (string=? str "à")
         (string=? str "â")
         (string=? str "ã"))
     "a"]
    [(or (string=? str "é")
         (string=? str "è")
         (string=? str "ê"))
     "e"]
    [(or (string=? str "í")
         (string=? str "ì")
         (string=? str "î"))
     "i"]
    [(or (string=? str "ó")
         (string=? str "ò")
         (string=? str "ô")
         (string=? str "õ"))
     "o"]
    [(or (string=? str "ú")
         (string=? str "ù")
         (string=? str "û"))
     "u"]
    [(string=? str "ç") "c"] 
    [else str]))

#| Especificação:
list -> list

Recebe uma lista de string e remove os caracteres especiais dos elementos.
|#

(define (remove-special-character-list string-list)
  (cond
    [(empty? string-list) empty]
    [else
     (cons
      (remove-special-character-string (first string-list))
      (remove-special-character-list (rest string-list)))]))


#| Especificação:
string -> string

Recebe uma string de uma letra do alfabeto e retorna a mesma letra em minúsculo, além de tratar o
caso da letra "ç".
|#

(define (to-lower-string str)
  (cond
    [(string=? str "A") "a"]
    [(string=? str "B") "b"]
    [(string=? str "C") "c"]
    [(string=? str "Ç") "c"]
    [(string=? str "D") "d"]
    [(string=? str "E") "e"]
    [(string=? str "F") "f"]
    [(string=? str "G") "g"]
    [(string=? str "H") "h"]
    [(string=? str "I") "i"]
    [(string=? str "J") "j"]
    [(string=? str "K") "k"]
    [(string=? str "L") "l"]
    [(string=? str "M") "m"]
    [(string=? str "N") "n"]
    [(string=? str "O") "o"]
    [(string=? str "P") "p"]
    [(string=? str "Q") "q"]
    [(string=? str "R") "r"]
    [(string=? str "S") "s"]
    [(string=? str "T") "t"]
    [(string=? str "U") "u"]
    [(string=? str "V") "v"]
    [(string=? str "W") "w"]
    [(string=? str "X") "x"]
    [(string=? str "Y") "y"]
    [(string=? str "Z") "z"]
    [else str]))

#| Especificação:
list -> list

Recebe uma lista de string e retorna a lista em lower case, ou seja, com todos os elementos em minúsculo.
|#

(define (to-lower-list string-list)
  (cond
    [(empty? string-list) empty]
    [else
     (cons
      (to-lower-string (first string-list))
      (to-lower-list (rest string-list)))]))

#| Especificação:
list -> list

Recebe uma lista de string e a retorna sem o último elemento.
|#

(define (remove-last-element string-list)
  (cond
    [(empty? string-list) empty]
    [(empty? (rest string-list)) empty]
    [else
     (cons (first string-list) (remove-last-element (rest string-list)))])
  )

#| Especificação:
list -> list

Recebe uma lista de string e retorna apenas a sub-lista do intervalo início + 1 até fim - 1.
|#

(define (without-extremes string-list)
  (remove-last-element (rest string-list)))

#| Especificação:
list -> boolean

Recebe uma lista de string e retorna true caso seja palíndromo e false caso contrário.
|#

(define (is-palindrome? string-list)
  (cond
    [(empty? string-list) #t]
    [(empty? (rest string-list)) #t]
    [else (and (equal? (first string-list) (last string-list))
               (is-palindrome? (without-extremes string-list)))]))

#| Especificação:
string -> boolean

Recebe uma string, transforma ela em uma lista de string normalizada e verifica
se é palíndromo ou não.
|#

(define (process-string str)
  (define normalized-list (normalize-string-list (remove-special-character-list (to-lower-list (string-split str "")))))
  (is-palindrome? normalized-list))


(define lower-letters-and-numbers (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                                        "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
                                        "w" "x" "y" "z" "1" "2" "3" "4" "5" "6" "7"
                                        "8" "9" "0"))

; Verificações

(examples
 (check-equal?
  (process-string "Aí, Lima falou: 'Olá, família!'.") #t)
 (check-equal?
  (process-string "Amo Omã. Se Roma me tem amores, amo Omã!") #t)
 (check-equal?
  (process-string "Luza Rocelina, a namorada do Manuel, leu na moda da romana: 'anil é cor azul'.") #t)
 (check-equal?
  (process-string "Aibofobia") #t)
 (check-equal?
  (process-string "ab123321ba") #t)
 (check-equal?
  (process-string "") #t)
 (check-equal?
  (process-string "Definitivamente não é um palíndromo.") #f)
 (check-equal?
  (process-string "E então ele disse: 'eu não sei o que é um palíndromo!'") #f)
 (check-equal?
  (process-string "12") #f)
 (check-equal?
  (process-string "2Aibofobia1") #f)
 )