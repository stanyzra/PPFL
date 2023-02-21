#lang racket
(require examples)
#|
Aluno: Stany Helberth de Souza Gomes da Silva
RA: 112683
|#

#| Uma determinada sala de reunião pode ser usada das 8:00h às 18:00h. Cada interessado em utilizar a sala
faz uma reserva indicando o intervalo de tempo que gostaria de utilizar a sala. Como parte de um sistema de
reservas, você deve projetar uma função que verifique se duas reservas podem ser atendidas, ou seja, não têm
conflito de horário.
|#

#| Análise:
Verificar se duas reservas podem ser atendidas no intervalo das 8:00h às 18:00h, evitando conflito
entre as reservas.
|#

; ##############################################################################################

#| Definição dos tipos de dados:
Horário (schedule) é uma estrutura de dados contendo as seguintes propriedades:
  - Hora (hour) é um número inteiro positivo de 0 à 23
  - Minuto (minute) é um número inteiro positivo 0 à 59

Reserva (Booking) é uma estrutura de dados contendo as seguintes propriedades:
  - Horario-Início (start-sched) é uma estrutura do tipo Horário (schedule) representando o horário
  de início
  - Horario-Encerramento (end-sched) é uma estrutura do tipo Horário (schedule) representando o horário
  de encerramento
|#

; ##############################################################################################

#| Especificação:
Horário Horário -> boolean

Recebe o ínicio e o fim da Reserva por parâmetro e valida se a o horário está informado
corretamente, ou seja, se o horário de início é menor que o horário de encerramento
da reserva. Caso as horas dos horários de início e encerramento sejam iguais, então
os minutos do encerramento deve ser maior que os minutos de início.
|#

(define (is-schedule-valid? start-sched end-sched)
  (and
   (<=
    (Schedule-hour start-sched)
    (Schedule-hour end-sched))
   (or
    (and
     (=
      (Schedule-hour start-sched)
      (Schedule-hour end-sched))
     (<
      (Schedule-minute start-sched)
      (Schedule-minute end-sched)))
    (<
     (Schedule-hour start-sched)
     (Schedule-hour end-sched)))))

#| Especificação:
Reserva Reserva -> boolean

Recebe as duas reservas (Booking) e verifica se há conflito de horas. Para evitar conflito
de horário, deve-se seguir as seguintes regras:
   
   início-hora-1 < início-hora-2 && início-hora-2 < término-hora-1
   início-hora-2 < início-hora-1 && início-hora-1 < término-hora-2
   início-hora-2 == início-hora-2 && início-hora-2 != término-hora-1
   início-minuto-2 < início-minuto-1 && (início-hora-1 == término-hora-2 || início-hora-2 == término-hora-1)

Assume-se que as informações providas são corretas, ou seja,
que os horários das duas reservas estejam entre o intervalo das 8:00h às 18:00h.
|#

(define (bookings-has-conflicts? booking-1 booking-2)
  (or
   (and
    (<
     (Schedule-hour (Booking-start-sched booking-1))
     (Schedule-hour (Booking-start-sched booking-2)))
    (<
     (Schedule-hour (Booking-start-sched booking-2))
     (Schedule-hour (Booking-end-sched booking-1)))
    )
   (and
    (<
     (Schedule-hour (Booking-start-sched booking-2))
     (Schedule-hour (Booking-start-sched booking-1)))
    (<
     (Schedule-hour (Booking-start-sched booking-1))
     (Schedule-hour (Booking-end-sched booking-2)))
    )
   (and
    (=
     (Schedule-hour (Booking-start-sched booking-2))
     (Schedule-hour (Booking-start-sched booking-1)))
    (not
     (=
      (Schedule-hour (Booking-start-sched booking-2))
      (Schedule-hour (Booking-end-sched booking-1)))
     ))
   (and
    (<
     (Schedule-minute (Booking-start-sched booking-2))
     (Schedule-minute (Booking-end-sched booking-1))
     )
    (or
     (=
      (Schedule-hour (Booking-start-sched booking-1))
      (Schedule-hour (Booking-end-sched booking-2))
      )
     (=
      (Schedule-hour (Booking-start-sched booking-2))
      (Schedule-hour (Booking-end-sched booking-1))
      )
     )
    )
   ))

#| Especificação:
Reserva Reserva -> String

Recebe as duas reservas (Booking) e retorna se há ou não conflito entre os horários,
bem como a tratativa de um formato inválido de horário. Ex.: início as 12:00 e término
as 11:00.

Assume-se que as informações providas são corretas, ou seja,
que os horários das duas reservas estejam entre o intervalo das 8:00h às 18:00h.
|#

(define (validate-bookings-schedule booking-1 booking-2)
  (if
   (and
    (is-schedule-valid? (Booking-start-sched booking-1) (Booking-end-sched booking-1))
    (is-schedule-valid? (Booking-start-sched booking-2) (Booking-end-sched booking-2)))
   (if (bookings-has-conflicts? booking-1 booking-2)
       "Há conflito nos horários das reservas."
       "Não há conflito nos horários das reservas.")
   "Horários formatados de forma inválida."))

(struct Schedule (hour minute) #:transparent)
#| schedule representa um horário de uma reserva
  - hour: número inteiro que varia de 0 a 23
  - minute: número inteiro que varia de 0 a 59
|#

(struct Booking (start-sched end-sched) #:transparent)
#| booking representa uma reserva
  - start-sched: horário (schedule) de ínicio
  - end-sched: horário (schedule) de encerramento
|#

; Verificações

(examples
 (check-equal?
  (validate-bookings-schedule
   (Booking (Schedule 12 30) (Schedule 11 00))
   (Booking (Schedule 15 30) (Schedule 18 00)))
  "Horários formatados de forma inválida.")
 (check-equal?
  (validate-bookings-schedule
   (Booking (Schedule 08 00) (Schedule 11 00))
   (Booking (Schedule 15 30) (Schedule 13 00)))
  "Horários formatados de forma inválida.")
 (check-equal?
  (validate-bookings-schedule ; metade do booking-2 dentro do booking-1
   (Booking (Schedule 8 00) (Schedule 11 35))
   (Booking (Schedule 10 35) (Schedule 15 40)))
  "Há conflito nos horários das reservas.")
 (check-equal?
  (validate-bookings-schedule ; metade do booking-1 dentro do booking-2
   (Booking (Schedule 12 40) (Schedule 17 20))
   (Booking (Schedule 8 30) (Schedule 13 35)))
  "Há conflito nos horários das reservas.")
 (check-equal?
  (validate-bookings-schedule ; booking-2 inteiramente dentro de booking-1
   (Booking (Schedule 08 00) (Schedule 15 00))
   (Booking (Schedule 08 10) (Schedule 14 50)))
  "Há conflito nos horários das reservas.")
 (check-equal?
  (validate-bookings-schedule ; booking-1 inteiramente dentro de booking-2
   (Booking (Schedule 08 10) (Schedule 14 50))
   (Booking (Schedule 08 00) (Schedule 15 00)))
  "Há conflito nos horários das reservas.")
 (check-equal?
  (validate-bookings-schedule ; booking-1 antes de booking-2
   (Booking (Schedule 08 00) (Schedule 12 00))
   (Booking (Schedule 13 30) (Schedule 15 00)))
  "Não há conflito nos horários das reservas.")
 (check-equal?
  (validate-bookings-schedule ; booking-2 antes de booking-1
   (Booking (Schedule 15 00) (Schedule 17 00))
   (Booking (Schedule 09 30) (Schedule 11 00)))
  "Não há conflito nos horários das reservas.")
 (check-equal?
  (validate-bookings-schedule ; booking-2 começando assim que booking-1 acaba
   (Booking (Schedule 09 00) (Schedule 10 30))
   (Booking (Schedule 10 30) (Schedule 15 30)))
  "Não há conflito nos horários das reservas.")
 (check-equal?
  (validate-bookings-schedule ; booking-1 começando assim que booking-2 acaba
   (Booking (Schedule 14 40) (Schedule 15 30))
   (Booking (Schedule 15 30) (Schedule 15 50)))
  "Não há conflito nos horários das reservas.")
 )

; ########################################################################################################################################
; ########################################################################################################################################

#|
Considere um jogo onde o personagem está em um tabuleiro (semelhante a um tabuleiro de jogo de xadrez).
As linhas e colunas do tabuleiro são enumeradas de 1 a 10, dessa forma, é possível representar a posição (casa)
do personagem pelo número da linha e da coluna que ele se encontra. O personagem fica virado para uma das
direções: norte, sul, leste ou oeste. O jogador controla o personagem através de um dos seguintes comandos:
virar a esquerda e virar a direita, que mudam a direção que o personagem está virado e avançar n casas, que faz
o personagem avançar até n casas na direção que ele está virado.
Projete uma função, incluindo os tipos de dados necessários, que receba como entrada o personagem do jogo e
um comando e gere como saída o novo estado do personagem.
Por exemplo, ao executar o comando para virar a direita, sendo que o personagem está na posição (1, 5) e virado
para o norte, a função deve gerar como resultado o personagem na posição (1, 5) virado para o leste.
Se o comando for para avançar duas casas, sendo que o personagem está na posição (7, 5) virado para o oeste,
a função deve gerar como resultado o personagem na posição (7, 3) virado para o oeste.
Dica: use papel e lápis para criar exemplos!
|#

#| Análise:
Receber como entrada o estado atual do personagem do jogo e um comando, gerando como saída o novo estado do personagem
do jogo. O ambiente em que o personagem se encontra é um plano cartesiano contendo o eixo horizontal (x) e o eixo vertical
(y), admitindo que ambos eixos vão de 1 a 10. O estado do personagem é composto na sua direção (norte, sul, leste e oeste)
e na sua posição, representado por um par ordenado (x,y) no plano cartesiano. O personagem não pode ultrapassar o limite
do ambiente, por exemplo, se ele estiver na posição (10, x), onde x pode ser qualquer número de 1 a 10, ele não pode andar
nenhuma casa virado para o norte.
|#

; ##############################################################################################

#| Definição dos tipos de dados:
Ação (action) é uma estrutura de dados que contém uma propriedade representada pela união de outros
dois tipos de dados.
  - type é uma união de estrutura de dados, podendo ser do tipo Virar-Lado (turn-sideways) ou
Andar (walk)

Posição (position) é uma estrutura de dados contendo as seguintes propriedades:
  - x é um valor inteiro de 1 a 10
  - y é um valor inteiro de 1 a 10

Estado-Personagem (character-status) é uma estrutura de dados contendo as seguintes propriedades:
  - Direção-Personagem (character-direction) é uma string representando a direção em que o personagem
  está virado em relação ao ambiente.
  - Posição-Personagem (character-position) é uma estrutura do tipo Posição (position) representando
    a localização do personagem no ambiente.

Virar-Lado (turn-sideways) é uma estrutura de dados contendo as seguintes propriedades:
  - actual-direction é uma string enumerada que admite uma das direções do ambiente (norte, sul,
    leste ou oeste).
  - side é uma string que representa a direção em que o personagem irá virar, podendo ser esquerda ou
    direita.

Andar (walk) é uma estrutura de dados contendo as seguintes propriedades:
  - actual-direction é uma string enumerada que admite uma das direções do ambiente (norte, sul,
    leste ou oeste).
  - n-steps é um int que representa o número de casas que o personagem irá andar.
|#

#| Especificação:
String String -> String

Recebe a posição atual do personagem e o lado em que ele irá virar, retornando a nova direção dependendo de qual
lado foi escolhido.
|#

(define (set-new-character-direction actual-character-position side)
  (cond
    [(string=? actual-character-position "norte")
     (if (string=? side "direita") "leste" "oeste")]
    [(string=? actual-character-position "leste")
     (if (string=? side "direita") "sul" "norte")]
    [(string=? actual-character-position "sul")
     (if (string=? side "direita") "oeste" "leste")]
    [else
     (if (string=? side "direita") "norte" "sul")]
    ))

#| Especificação:
Posição string int -> Posição

Recebe a posição (Position) atual e a direção do personagem e retorna a nova posição (Position)
somada com o número de passos recebido. Se a direção for leste ou oeste, a soma será feita no eixo x,
caso contrário, y.

Assume-se que as informações providas são corretas, ou seja, que o personagem não irá ultrapassar
o limite do ambiente (0 < x,y <= 10).
|#

(define (set-new-character-position current-character-position character-direction n-steps)
  (cond
    [(string=? character-direction "norte")
     (struct-copy Position current-character-position
                  [y (+ (Position-y current-character-position) n-steps)])]
    [(string=? character-direction "sul")
     (struct-copy Position current-character-position
                  [y (- (Position-y current-character-position) n-steps)])]
    [(string=? character-direction "leste")
     (struct-copy Position current-character-position
                  [x (+ (Position-x current-character-position) n-steps)])]
    [else
     (struct-copy Position current-character-position
                  [x (- (Position-x current-character-position) n-steps)])]
    ))

#| Especificação:
Estado-Personagem Ação -> Estado-Personagem

Recebe o estado atual do personagem (Character-Status) e a ação (Action), retornando o novo
estado do personagem dependendo do tipo da ação escolhida. Se a ação for mudar de direção
(Turn-Sideways), ele poderá virar a esquerda ou a direita da posição atual. Caso a ação for andar
(Walk), ele poderá andar um determinado número de casas passado por parâmetro.

Assume-se que as informações providas são corretas, ou seja, que o personagem não irá ultrapassar
o limite do ambiente (0 < x,y <= 10).
|#

(define (change-character-state current-character-status character-action)
  (cond
    [(Turn-Sideways? character-action)
     (struct-copy
      Character-Status current-character-status
      [character-direction
       (set-new-character-direction
        (Turn-Sideways-actual-direction character-action)
        (Turn-Sideways-side character-action))]
      )]
    [(Walk? character-action)
     (define new-character-position
       (set-new-character-position
        (Character-Status-character-position current-character-status)
        (Walk-actual-direction character-action)
        (Walk-n-steps character-action)))
     (struct-copy
      Character-Status current-character-status
      [character-position
       (Position
        (Position-x new-character-position)
        (Position-y new-character-position))]
      )
     ]
    ))

(struct Action (type) #:transparent)
#| action representa a ação do personagem, ou seja, virar ou andar.

  - type: Turn-Sideways | Walk

  O atributo type pode é uma união de dois casos, podendo ser
  do tipo Turn-Sideways (virar) ou Walk (andar).
|#

(struct Turn-Sideways (actual-direction side) #:transparent)
#| turn-sideways representa a direção em que o personagem irá virar
  - actual-direction: direção em que o personagem está virado
  - side: lado para virar (esquerda ou direita)
|#

(struct Walk (actual-direction n-steps) #:transparent)
#| walk representa o número de casas em que o jogador irá avançar 
  - actual-direction: direção em que o personagem está virado
  - n-steps: número de passos a ser andado
|#

(struct Position (x y) #:transparent)
#| position representa a posição do personagem no ambiente
  - x: número inteiro de 1 a 10
  - y: número inteiro de 1 a 10
|#

(struct Character-Status (character-direction character-position) #:transparent)
#| character-status representa o estado do personagem no ambiente
  - character-direction: direção em que o personagem está virado
  - character-position: posição (position) do personagem no ambiente
|#

; Verificações

(examples
 (check-equal?
  (change-character-state
   (Character-Status "norte" (Position 1 1))
   (Walk "norte" 1))
  (Character-Status "norte" (Position 1 2)))
 (check-equal?
  (change-character-state
   (Character-Status "leste" (Position 2 4))
   (Walk "leste" 4))
  (Character-Status "leste" (Position 6 4)))
 (check-equal?
  (change-character-state
   (Character-Status "sul" (Position 5 5))
   (Walk "sul" 4))
  (Character-Status "sul" (Position 5 1)))
 (check-equal?
  (change-character-state
   (Character-Status "oeste" (Position 4 8))
   (Walk "oeste" 1))
  (Character-Status "oeste" (Position 3 8)))
 (check-equal?
  (change-character-state
   (Character-Status "norte" (Position 1 1))
   (Turn-Sideways "norte" "direita"))
  (Character-Status "leste" (Position 1 1)))
 (check-equal?
  (change-character-state
   (Character-Status "sul" (Position 4 6))
   (Turn-Sideways "sul" "esquerda"))
  (Character-Status "leste" (Position 4 6)))
 (check-equal?
  (change-character-state
   (Character-Status "oeste" (Position 4 8))
   (Turn-Sideways "oeste" "direita"))
  (Character-Status "norte" (Position 4 8)))
 (check-equal?
  (change-character-state
   (Character-Status "norte" (Position 8 6))
   (Turn-Sideways "norte" "esquerda"))
  (Character-Status "oeste" (Position 8 6)))
 (check-equal?
  (change-character-state
   (Character-Status "sul" (Position 10 10))
   (Turn-Sideways "sul" "direita"))
  (Character-Status "oeste" (Position 10 10)))
 (check-equal?
  (change-character-state
   (Character-Status "oeste" (Position 1 7))
   (Turn-Sideways "oeste" "esquerda"))
  (Character-Status "sul" (Position 1 7)))
 )

; ########################################################################################################################################
; ########################################################################################################################################

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
list int -> int

Recebe uma lista de string e o maior número de casas decimais da lista e retorna uma nova lista
de string com todos os elementos contendo o mesmo número de caracteres.
|#

(define (append-spaces string-list higher-n-length)
  (cond
    [(empty? string-list) empty]
    [else
     (define actual-number-length (string-length (first string-list)))
     (define n-characters-left (- higher-n-length actual-number-length))
     (define new-element (string-append (first string-list) (make-string n-characters-left #\space)))
     (cons new-element
           (append-spaces (rest string-list) higher-n-length))
     ]))


#| Especificação:
list int -> int

Recebe uma lista de string e o número de casas decimais de um determinado elemento na lista,
retornando o número de caracteres da string que possui o maior número de casas decimais.
|#

(define (get-higher-n-length string-list number-length)
  (cond
    [(empty? string-list) number-length]
    [else
     (define actual-number-length (string-length (first string-list)))
     (get-higher-n-length
      (rest string-list)
      (if
       (> actual-number-length number-length)
       actual-number-length
       number-length))
     ]))

#| Especificação:
list -> list

Recebe uma lista de números e retorna uma nova lista com todos os elementos convertidos
para string.
|#

(define (cast-list-to-string number-list)
  (cond
    [(empty? number-list) empty]
    [else
     (cons (number->string (first number-list))
           (cast-list-to-string (rest number-list)))])
  )

#| Especificação:
list -> list

Recebe uma lista de números e retorna uma lista de string com o mesmo número de
caracteres para cada elemento.
|#

(define (convert-list number-list)
  (define converted-string-list
    (cast-list-to-string number-list))
  (define higher-n-length
    (get-higher-n-length
     converted-string-list
     (string-length
      (first converted-string-list))))
  (append-spaces converted-string-list higher-n-length))

(examples
 (check-equal?
  (convert-list (list 1 2 33 66 884))
  (list "1  " "2  " "33 " "66 " "884"))
 (check-equal?
  (convert-list (list 4 16548 2 8))
  (list "4    " "16548" "2    " "8    "))
 (check-equal?
  (convert-list (list 55555 4444 484848 123485 2))
  (list "55555 " "4444  " "484848" "123485" "2     "))
 (check-equal?
  (convert-list (list 1 3 2 4 2))
  (list "1" "3" "2" "4" "2"))
 (check-equal?
  (convert-list (list 1 1 1 8000 1 1 1 1))
  (list"1   " "1   " "1   " "8000" "1   " "1   " "1   " "1   ")) 
 )

; ########################################################################################################################################
; ########################################################################################################################################

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

