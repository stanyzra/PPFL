#lang racket
#| Uma determinada sala de reunião pode ser usada das 8:00h às 18:00h. Cada interessado em utilizar a sala
faz uma reserva indicando o intervalo de tempo que gostaria de utilizar a sala. Como parte de um sistema de
reservas, você deve projetar uma função que verifique se duas reservas podem ser atendidas, ou seja, não têm
conflito de horário.
|#

#| Análise:
Verificar se duas reservas podem ser atendidas no intervalo das 8:00h às 18:00h. Não há
conflito de horário caso as duas reservas possam ser atendidas dentre o intervalo de tempo
estabelecido.
|#

; ##############################################################################################

#| Definição dos tipos de dados:
Reserva (schedule) é uma estrutura de dados contendo as seguintes propriedades:
  - início (start) é um número inteiro positivo de quatro dígitos
  - fim (end) é um número inteiro positivo de quatro dígitos
Os dois primeiros dígitos referem-se à hora, enquanto que os dois restantes referem-se
aos minutos da reserva.
|#

; ##############################################################################################

#| Especificação:
Reserva -> boolean

Recebe uma Reserva por parâmetro e verifica se é os horários especificados
respeitam o intervalo de disponibilidade da sala. Retorna T ou F.
|#

(define (isAvailable? sched)
  (if (and (<= (Schedule-start sched) (Schedule-end sched))
           (and (and (>= (Schedule-start sched) 0800) (<= (Schedule-start sched) 1800))
                (and (>= (Schedule-end sched) 0800) (<= (Schedule-end sched) 1800))))
      #t
      #f))

(struct Schedule (start end) #:transparent)
#| schedule representa uma reserva
  - start: número inteiro de quatro dígitos representando o ínicio da reserva
  - end: número inteiro de quatro dígitos representando o fim da reserva
|#

(define sched1 (Schedule 0800 1100))
(define sched2 (Schedule 1300 1700))

(if (and (isAvailable? sched1) (isAvailable? sched2)) "ok" "merda")