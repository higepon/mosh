SERVICE_NAME=archlinux_amd64

build:
	docker-compose build $(SERVICE_NAME)

start: build
	docker-compose up -d --build

stop:
	docker-compose stop $(SERVICE_NAME)

attach: start
	docker-compose exec $(SERVICE_NAME) bash

push:
# TODO
#	docker push higepon/mosh-bootstrap

clean:
	docker-compose down --rmi all
