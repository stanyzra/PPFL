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