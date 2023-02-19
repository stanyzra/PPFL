#lang racket
(require examples)

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
Direção (direction) é uma estrutura de dados contendo as seguintes propriedades:
  - Norte (north) é um valor boolean
  - Sul (south) é um valor boolean
  - Leste (east) é um valor boolean
  - Oeste (west) é um valor boolean
Se algum dos valores for #t, então o personagem está virado para a direção especificada, caso contrário,
não. O personagem só pode estar virado para uma direção, ou seja, se uma propriedade for #t, todas as outras
são #f.

Posição (position) é uma estrutura de dados contendo as seguintes propriedades:
  - x é um valor inteiro de 1 a 10
  - y é um valor inteiro de 1 a 10

Estado-Personagem (character-status) é uma estrutura de dados contendo as seguintes propriedades:
  - Direção-Personagem (character-direction) é uma estrutura do tipo Direção (direction) representando
    a direção em que o personagem está virado.
  - Posição-Personagem (character-position) é uma estrutura do tipo Posição (position) representando
    a localização do personagem no ambiente.
|#

#| Especificação:
Posição Direção número-passos -> Posição

Recebe a posição (Position) atual e a nova direção (Direction) do personagem e retorna a nova posição (Position)
somada com o número de passos recebido. Se a direção for leste ou oeste, a soma será feita no eixo x,
caso contrário, y.

Assume-se que as informações providas são corretas, ou seja, que o personagem não irá ultrapassar
o limite do ambiente (0 < x,y <= 10).
|#

(define (set-new-character-position current-character-position new-character-direction n-steps)
  (cond
    [(Direction-north new-character-direction)
     (struct-copy Position current-character-position
                  [y (+ (Position-y current-character-position) n-steps)])]
    [(Direction-south new-character-direction)
     (struct-copy Position current-character-position
                  [y (- (Position-y current-character-position) n-steps)])]
    [(Direction-east new-character-direction)
     (struct-copy Position current-character-position
                  [x (+ (Position-x current-character-position) n-steps)])]
    [else
     (struct-copy Position current-character-position
                  [x (- (Position-x current-character-position) n-steps)])]
    ))

#| Especificação:
Estado-Personagem Direção número-passos -> Estado-Personagem

Recebe o estado atual do personagem (Character-Status), a direção (Direction) futura e o número de passos
que o personagem deve andar, retornando o novo estado do personagem (Character-Status)

Assume-se que as informações providas são corretas, ou seja, que o personagem não irá ultrapassar
o limite do ambiente (0 < x,y <= 10).
|#

(define (change-character-state current-character-status new-character-direction n-steps)
  (define new-character-position
    (set-new-character-position
     (Character-Status-character-position current-character-status)
     new-character-direction
     n-steps))
  (define new-character-status (struct-copy
                                Character-Status current-character-status
                                [character-direction
                                 (Direction
                                  (Direction-north new-character-direction)
                                  (Direction-south new-character-direction)
                                  (Direction-east new-character-direction)
                                  (Direction-west new-character-direction))]
                                [character-position
                                 (Position
                                  (Position-x new-character-position)
                                  (Position-y new-character-position))]
                                ))
  ;(Position-y (Character-Status-character-position new-character-status)))
  new-character-status)

(struct Direction (north south east west) #:transparent)
#| direction representa a direção em que o personagem está virado
  - north: boolean
  - south: boolean
  - east: boolean
  - west: boolean
|#

(struct Position (x y) #:transparent)
#| position representa a posição do personagem no ambiente
  - x: número inteiro de 1 a 10
  - y: número inteiro de 1 a 10
|#

(struct Character-Status (character-direction character-position) #:transparent)
#| character-status representa o estado do personagem no ambiente
  - character-direction: direção (direction) em que o personagem está virado
  - character-position: posição (position) do personagem no ambiente
|#

; Verificações

(examples
 (check-equal?
  (change-character-state
   (Character-Status (Direction #t #f #f #f) (Position 1 1))
   (Direction #f #f #t #f) 1)
  (Character-Status (Direction #f #f #t #f) (Position 2 1)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #t #f #f #f) (Position 5 3))
   (Direction #t #f #f #f) 4)
  (Character-Status (Direction #t #f #f #f) (Position 5 7)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #t #f #f #f) (Position 3 10))
   (Direction #f #t #f #f) 9)
  (Character-Status (Direction #f #t #f #f) (Position 3 1)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #t #f #f #f) (Position 1 3))
   (Direction #f #f #t #f) 9)
  (Character-Status (Direction #f #f #t #f) (Position 10 3)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #t #f #f #f) (Position 6 2))
   (Direction #f #f #f #t) 3)
  (Character-Status (Direction #f #f #f #t) (Position 3 2)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #f #f #f #t) (Position 5 5))
   (Direction #f #f #t #f) 0)
  (Character-Status (Direction #f #f #t #f) (Position 5 5)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #f #t #f #t) (Position 5 10))
   (Direction #f #t #f #f) 9)
  (Character-Status (Direction #f #t #f #f) (Position 5 1)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #f #f #t #f) (Position 2 3))
   (Direction #t #f #f #f) 5)
  (Character-Status (Direction #t #f #f #f) (Position 2 8)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #t #f #f #f) (Position 10 5))
   (Direction #t #f #f #f) 5)
  (Character-Status (Direction #t #f #f #f) (Position 10 10)))
 (check-equal?
  (change-character-state
   (Character-Status (Direction #f #f #t #f) (Position 4 6))
   (Direction #f #f #f #t) 2)
  (Character-Status (Direction #f #f #f #t) (Position 2 6)))
 )


