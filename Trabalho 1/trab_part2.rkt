#lang racket
(require examples)

#|
Projete uma função que receba como entrada uma lista de números e produza uma lista com os números
convertidos para string de maneira que todas as strings tenham a mesma quantidade de caracteres (use espaço
para completar os números se necessário).
|#

#|
Análise:
Projetar um programa que transforme uma lista de números em uma lista de string, fazendo com que todas as
strings tenham a mesma quantidade de caracteres. Pode-se adotar a estratégia de procurar o maior número na
lista e usa-lo como base para completar o resto das strings, preenchendo as casas decimais inexistentes com
espaço.
|#

; ##############################################################################################

#| Definição dos tipos de dados:
Lista-Numeros (number-list) é uma lista contendo números.

Lista-String-Convertida (converted-string-list) é uma lista contendo os números da Lista-Numeros
(number-list) em formato string, com os espaços preenchidos dependendo do número de casas decimais
que um determinado número dentro da lista possui.
|#

; ##############################################################################################

#| Especificação:
list int -> list

Concatena os espaços necessários para que todos os elementos da lista fiquem com o
mesmo número de caracteres. Retorna uma lista.
|#

(define (append-spaces number-list higher-n-string)
  higher-n-string)

#| Especificação:
list int -> list int (?)

Recebe uma lista de números e o número de caracteres do primeiro da lista e retorna
uma nova lista com os números convertidos para string e o maior número de caracteres
da lista.
|#

(define (convert-list number-list higher-n-string)
  (cond
    [(empty? number-list) (append-spaces number-list higher-n-string)]
    [else
     (define actual-string-length (string-length (number->string (first number-list))))
     (cons (number->string (first number-list))
           (convert-list (rest number-list)
                         (if (> higher-n-string actual-string-length) higher-n-string actual-string-length)))])
                         ;(string-length (number->string (first number-list)))))])
  )

(struct List-Info (list higher-n-char) #:transparent)

(define number-list (list 1 2 33 66 884))
(convert-list number-list (string-length (number->string (first number-list))))
;(define converted-string-list (convert-list number-list (string-length (number->string (first number-list)))))
;converted-string-list
