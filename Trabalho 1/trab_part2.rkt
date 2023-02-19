#lang racket
(require examples)

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


