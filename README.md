<div align="center">
  <h1 align="center">Backend Web com Scotty 🖥️</h1> 
  <h3 align="center">Produção Individual - Paradigmas de programação</h3>
  <p align="center">Universidade Federal de Santa Maria<br><br>Aluno: Cauã Welter da Silva | Curso: Sistemas de Informação</p>
</div>

## Objetivo 🎯

Para este trabalho, pretende-se desenvolver um serviço web que simule o funcionamento de um mercado virtual similiar ao do jogo eletrônico "Warframe", utilizando o framework Scotty. O serviço possibilitará que um usuário possa conferir uma lista de itens disponíveis para a compra, bem como consultar categorias específicas de itens e quais itens podem ser adquiridos com determinada quantidade de dinheiro. O usuário deve informar ao serviço o que deseja consultar, podendo combinar especificações de filtragem para obter exatamente a listagem de itens que deseja.

## Desenvolvimento 🧑‍💻

### Etapa 1: Entendendo servidores Web 🛜

Meu primeiro objetivo com a produção deste trabalho foi **compreender o básico do funcionamento de serviços e servidores Web**. Com pouco conhecimento sobre o assunto, esta se demonstrou uma primeira etapa importante para compreender o que está sendo produzido para esta atividade.  
O código a ser construído utilizando o framework Scotty funcionaria como um servidor Web hospedado localmente na máquina onde o código está rodando; sendo assim, a única maneira de utilizar o serviço é na própria máquina onde atualmente o código está rodando. Serviços Web como este são usualmente armazenados em grandes computadores (servidores) que guardam todo o código que o compõe. Estes servidores estão conectados à internet e podem acessados através do seu nome de domínio (DNS). No caso do nosso pequeno serviço Scotty, este pode ser acessado através do endereço **localhost:3000**, usado para **aceder a uma aplicação ou serviço web que está a ser executado no nosso próprio computador**, através da porta 3000. ***localhost*** refere-se à **sua máquina**, e ***3000*** é o **número da porta** onde o servidor de desenvolvimento está a **escutar por conexões**. O serviço também necessita de um protocolo para possibilitar a obtenção de recursos do servidor Web. Nossa aplicação Scotty utiliza o **protocolo *HTTP*** para realizar esta **comunicação entre navegador e servidor**. Essa comunicação funciona através de uma **troca de mensagens**, onde o navegador envia *requests* e o servidor retorna *responses*

### Etapa 2: Construção 🔨

Durante a pesquisa, foi explicitado para mim no seguinte <a href="https://www.stackbuilders.com/insights/getting-started-with-haskell-projects-using-scotty/">tutorial</a> como é possível utilizar ferramentas em HTML para criar um front-end para a aplicação. Considerando minha experiência e interesse por criar páginas web com HTML e CSS, a opção de utilizar um front-end construído com essas duas linguagens pareceu extremamente viável. 

## Resultado

## Referências
https://youtu.be/psTTKGj9G6Y?si=NrH5j3mBDvDwkVyy - Build a Haskell Server with Scotty framework

https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Guides/Overview - Uma visão geral do HTTP

https://developer.mozilla.org/pt-BR/docs/Learn_web_development/Howto/Web_mechanics/What_is_a_web_server - O que é um servidor web (web server)?

https://www.stackbuilders.com/insights/getting-started-with-haskell-projects-using-scotty/ - Getting started with Haskell projects using Scotty 

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/7NMOLXjY)
