SERVICE_NAME=ubuntu_22.04

start:
	docker-compose up -d --build

stop:
	docker-compose stop $(SERVICE_NAME)

attach:
	docker-compose exec $(SERVICE_NAME) bash

push:
# TODO
#	docker push higepon/mosh-bootstrap

clean:
	docker-compose down --rmi all
