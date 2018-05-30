## MoniQue за 5 минут

Чтобы быстро познакомиться с MoniQue, достаточно пройтись по следующим шагам.

1. Скачать необходимые файлы для запуска:

```
mkdir mq
cd mq
wget https://raw.githubusercontent.com/biocad/mq/master/docker/docker-compose.yaml
wget https://raw.githubusercontent.com/biocad/mq/master/docker/config-compose.json
wget https://raw.githubusercontent.com/biocad/mq/master/docker/config-controller-compose.json
```

2. Проверить конфигурационные файлы. 
Если компонент должен запускаться с помощью [контроллера](Controller.md), то добавить необходимые поля в файл `config-controller-compose.json`, поле `connections`.

3. Запустить все необходимые компоненты с помощью инструмента [Docker](https://www.docker.com/):

```
docker-compose up -d
```

> Чтобы остановить и удалить запущенные предыдущей командой контейнеры, достаточно в этой же папке вызвать команду `docker-compose down`.


3. Запустить один из примеров на языке [haskell](https://github.com/biocad/mq-component-hs#%D0%97%D0%B0%D0%BF%D1%83%D1%81%D0%BA-%D0%BF%D1%80%D0%B8%D0%BC%D0%B5%D1%80%D0%BE%D0%B2) или [python](https://github.com/biocad/mq-component-py#%D0%97%D0%B0%D0%BF%D1%83%D1%81%D0%BA-%D0%BF%D1%80%D0%B8%D0%BC%D0%B5%D1%80%D0%BE%D0%B2).
