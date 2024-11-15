# Lista 1 -- Teoria da Computação / Linguagens Formais e Autômatos -- 2024/2

**Introdução à Teoria da Computação & Fundamentos Matemáticos**

**Autor:** Matheus Jagi

## Problema 4
Este problema pede a construção de um fluxo de entradas de conjuntos (nome -> valores) e a entrada de uma expressão $n$, podendo essa permitir as operações de conjunto, união, interseção, diferença, diferença simétrica, complemento, produto cartesiano e conjunto das partes.

## Solução
A solução adotada foi a criação de uma `trait` criando uma tipo SetExpr, que força a implementação de um método evaluate, onde para cada operação permitida é criada uma classe que retorna o valor do seu processamento. Com isso foi criada uma classe para realizar o processamento das operações, tendo como método principal o parse, tendo como entrada a expressão de conjuntos a ser processada. Essa operação processa a expressão através do uso de pattern para realizar as operações de precedência dos parenteses recursivamente. Cada operação é tokenizada, por exemplo, a expressão A & B viram uma lista de tokens List(A, &, B), recebida por um método de processamento da tokenização, fazendo o processamento recursivo das operações. Um método usado para guardar os valores precedentes do parenteses foi criar uma "variável-string" chamada `_TEMP[...]`, onde a mesma guarda o index do resultado da expressão, como por exemplo, _TEMP[0] guarda a primeira operação, sendo 0 o index de uma lista do tipo SetExpr que guarda o resultado da equação processada.

## Executando...

### Para executar usando o SBT
```bash
sbt run
```

O programa pedirá em primeiro lugar o nome do conjunto:

```bash
Insira o nome do conjunto de A a Z (ou 'c' para continuar):
A -> Informe o nome do seu conjunto
```

Em seguida, pedirá os valores dos conjuntos separados por virgula:

```bash
Agora insira os valores do conjunto [A] separados por virgula (ex.: 1,5,7):
1,2,3 -> Informe os valores do conjunto
```

Após isso, mostrará os valores dos conjuntos informadados até agora:

```bash
Os conjuntos até agora são:
(A -> Set(1,2,3))
```

Isso feito, digite ':c' no primeiro passo. O programa mostra uma lista com as operações permitidas e solicita a entrada da expressão a ser realizada:

```bash
Nossas operações permitidas são:
    -> União (A | B)
    -> Interseção (A & B)
    -> Diferença (A - B)
    -> Diferença simétrica (A ^ B)
    -> Complemento (~A)
    -> Produto cartesiano (A * B)
    -> Conjunto das partes (P(A))

Informe a expressão que deseja calcular para os conjuntos (ex.:  A | (B & C) -> Respeite os espacos eos conjuntos):
~((A & B) | (C - D)) -> Informe sua expressão
```

O programa realiza a expressão fornecendo o resultado:

```bash
O resultado da expressão [~((A & B) | (C - D))] e: [RESULTADO]
```

Caso deseja continuar com a execução do programa informando outras equações digite ENTER ou ':q' para encerrar o programa:

```bash
Para finalizar o programa digite 'q' ou ENTER para continuar
```
