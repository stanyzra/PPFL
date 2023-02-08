#lang racket
(require examples)

#| Uma determinada sala de reunião pode ser usada das 8:00h às 18:00h. Cada interessado em utilizar a sala
faz uma reserva indicando o intervalo de tempo que gostaria de utilizar a sala. Como parte de um sistema de
reservas, você deve projetar uma função que verifique se duas reservas podem ser atendidas, ou seja, não têm
conflito de horário.
|#

#| Análise:
Verificar se duas reservas podem ser atendidas no intervalo das 8:00h às 18:00h. Para evitar conflito
de horário, o horário de início de uma reserva não pode ser menor que os horários de encerramento e
maior que de início da outra reserva ao mesmo tempo.
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