## Projeto IA - Robô aspirador de pó
O problema do robô aspirador de pó consiste em 1 cenário matricial, 1 agente aspirador de pó (ADDP), 1 dockstation, 1 lixeira, N sujeiras, M paredes e O elevadores.

Ele visa tratar o problema de modelagem de estados e transição em IA. As restrições são:

- Posição inicial do AADP é definida pelo usuário;
- A movimentação do AADP é livre na horizontal e na vertical, exceto se:
 - Houver uma parede bloqueando
 - For margem do cenário
 - A movimentação do AADP na vertical só se dá por intermédio dos elevadores
 - O AADP deve coletar uma sujeira quando passar por ela
 - A cada duas sujeiras coletadas, o AADP deve esvaziar-se na lixeira
