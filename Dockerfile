FROM erlang:21.3

#Переопределяем рабочую директорию
WORKDIR /app

#Определяем IP локальной машины
ARG ip='192.168.1.1'

#Копирование директории приложения
COPY . .

#Подключение портов
EXPOSE 8080

#Установка программного обеспечения для администрирования
RUN apt-get update
RUN apt-get install -y vim curl
RUN apt-get install -y git
RUN apt-get install -y net-tools inetutils-ping

#Установка программного обеспечения для работы с аудиофайлами
RUN apt-get install -y ffmpeg libortp-dev
#Предоставляем доступ к исполняемому файлу
RUN chmod +x voice_client

#Обновляем зависимости проекта
RUN sed -i -e 's/IP_PBX_CHANGE/'$ip'/g' -e 's/xx/yy/g' sys.config
RUN rebar3 upgrade

#Точка начала работы WebRTP
ENTRYPOINT ["rebar3","shell"]