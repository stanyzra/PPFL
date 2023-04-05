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
iguais. Essa string pode contém letras do alfabeto, números e símbolos usados na Língua Portuguesa. Admitindo
que a estratégia de analisar a lista sem extremos seria mais complicado de implementar ao utilizar funções de
alta ordem, escolheu-se utilizar a estratégia de verificar se a lista de strings (normalizada, ou seja, contendo
apenas letras e números) é exatamente igual a ao reverso dela mesma. Assim, é possível implementar essa solução
ao fazer o uso de funções de alta ordem como map, filter, member e foldl.
|#

; ##############################################################################################

#| Definição dos tipos de dados:

letras-minusculas-e-numeros (lower-letters-and-numbers) é uma lista contendo todas as letras minúsculas do alfabeto da
Língua Portuguesa, bem como números de 0 a 9.

|#

; ##############################################################################################

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
string -> boolean

Recebe uma string, transforma ela em uma lista de string normalizada e verifica
se é palíndromo ou não.
|#

(define (process-string-higher-order str)
  ; transforma a string em uma lista de strings
  (define string-list (string-split str ""))
  ; mapeia a lista e devolve uma nova lista com as letras em lowercase
  (define lower-string-list (map to-lower-string string-list))
  ; mapeia a lista e devolve uma nova lista sem caracteres especiais
  (define removed-special-list (map remove-special-character-string lower-string-list))
  ; filtra a lista e normaliza a lista, removendo qualquer caractere que não seja uma letra ou um número
  (define normalized-list (filter
                           ; função anônima que retorna #f caso o elemento verificado não seja uma letra ou um número
                           (lambda (list-element) 
                                    (member list-element lower-letters-and-numbers)) removed-special-list))
  ; verifica se a lista normalizada é igual ao inverso dela mesma. Se for, é um palíndromo.
  (equal? normalized-list (foldl cons empty normalized-list)))

(define lower-letters-and-numbers (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                                        "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
                                        "w" "x" "y" "z" "1" "2" "3" "4" "5" "6" "7"
                                        "8" "9" "0"))
; Verificações

(examples
 (check-equal?
  (process-string-higher-order "Aí, Lima falou: 'Olá, família!'.") #t)
 (check-equal?
  (process-string-higher-order "Amo Omã. Se Roma me tem amores, amo Omã!") #t)
 (check-equal?
  (process-string-higher-order "Luza Rocelina, a namorada do Manuel, leu na moda da romana: 'anil é cor azul'.") #t)
 (check-equal?
  (process-string-higher-order "Aibofobia") #t)
 (check-equal?
  (process-string-higher-order "ab123321ba") #t)
 (check-equal?
  (process-string-higher-order "") #t)
 (check-equal?
  (process-string-higher-order "Definitivamente não é um palíndromo.") #f)
 (check-equal?
  (process-string-higher-order "E então ele disse: 'eu não sei o que é um palíndromo!'") #f)
 (check-equal?
  (process-string-higher-order "12") #f)
 (check-equal?
  (process-string-higher-order "2Aibofobia1") #f)
 )