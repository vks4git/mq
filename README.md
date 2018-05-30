# mq

*Если у вас нет времени на чтение полной документации - попробуйте посетить раздел [MoniQue за 5 минут](doc/5minutes.md)*.

## Репозитории MoniQue

Помимо данной библиотеки к системе MoniQue относятся и другие:
  * [mq-component-hs](https://github.com/biocad/mq-component-hs): реализует компонент на языке haskell;
  * [mq-component-py](https://github.com/biocad/mq-component-py): реализует компонент на языке python;
  * [mq-controller](https://github.com/biocad/mq-controller): реализует логику [контроллера](doc/Controller.md);
  * [mq-websocket](https://github.com/biocad/mq-websocket): реализует общение web-компонентов посредством websocket;
  * [mq-monitoring](https://github.com/biocad/mq-monitoring): реализует сбор информации о состоянии системы;
  * [mq-jobcontrol](https://github.com/biocad/mq-jobcontrol): реализует возможнось отладки и тестирования компонентов.

## Содержание репозитория mq

* [Введение](doc/Intro.md): основные идеи и положения.
* [Протокол](doc/Protocol.md): формат передаваемых сообщений.
* [Ошибки](doc/Error.md): формат ошибок.
* [Транспортный слой](doc/Transport.md): техническая реализация и паттерны общения.
* [Коммуникационный и технический слой](doc/CommAndTech.md): две части одного приложения.
* [Одно место (Scheduler)](doc/Scheduler.md): единое место синхронизации для всего в системе MoniQue.
* [Контроллер](doc/Controller.md): эффективнее выполняем тяжёлые задачи.
* [Конфигурационный файл (config.json)](doc/ConfigJson.md): параметры запуска приложения.
* [Разработка компонента на новом языке](doc/NewLanguage.md): что делать, если я не пишу на python или haskell?
* [Создание нового компонента](doc/Develop.md): что делать, если я хочу разработать новый компонент?
* [MoniQue за 5 минут](doc/5minutes.md): минимальный пример для знакомства.

