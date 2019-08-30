# Hapi-fhir-jpaserver-oauth

[![Build Status](https://travis-ci.org/AriHealth/hapi-fhir-jpaserver-oauth.svg?branch=master)](https://travis-ci.org/AriHealth/hapi-fhir-jpaserver-oauth) [![codecov.io](https://codecov.io/gh/AriHealth/hapi-fhir-jpaserver-oauth/branch/master/graphs/badge.svg)](http://codecov.io/gh/AriHealth/hapi-fhir-jpaserver-oauth)

## Description

Hapi-fhir-jpaserver-oauth is HAPI v 3.8.0-SNAPSHOT with support for mySQL and OAuth. 

## Technology

- Java 11
- Maven for Java dependency management
- Jetty
- Spring Framework 

## Functionalities

- Open-source implementation of the FHIR specification in Java based on [HAPI](http://hapifhir.io/).

## How to deploy

Compile and package the project with:

```
mvn clean install
```

and deploy with:

```
mvn jetty:run
```

Go to your browser and type http://localhost:8080/hapi

If you enable the OAuth capability (see next section) deploy the [Keycloak OAuth 2.0](https://github.com/AriHealth/keycloak-auth) and dependencies.

## Environment variables

The following environment variables must be set prior to the execution:

	DB_VENDOR=DERBY - List of possible vendors [DERBY,MYSQL,MARIADB]
	DB_HOST=localhost - Host where the database is deployed (it can be empty for DERBY)
	DB_PORT=3306 -  - Port where the database is deployed (it can be empty for DERBY)
	DB_USER=fhiruser - FHIR user for the database (it can be empty for DERBY)
	DB_PASSWORD=fhirpwd
	DB_DATABASE=fhirdb
	LUCENE_FOLDER=/var/lib/tomcat8/webapps/hapi/indexes - Place where the Lucene index are stored
	OAUTH_ENABLE=false - To enable/disable authentication
	OAUTH_URL=http://auth:8081 - In case of enabling authentication, the url where the Keycloak OAuth 2.0 is available

## Docker deployment

### Full stack deployment using docker-compose

The project comes with a [docker-compose](https://docs.docker.com/compose/) which deploys testing containers for Keycloak (port: 9090), HAPI (8080), OAuth 2.0 and the authenticator (8081):

0. Check the configuration values at the environment (`.env`) file. You can modify at your needs
1. Execute `docker-compose up -d`
2. Wait a couple of minutes until the stack is deployed. Check the logs with `docker logs --details hapi-fhir`
3. Access the Keycloak console `http://localhost:9090` (**user**: admin, **password**: Pa55w0rd)
* Create a realm `HAPIFHIR`
* Inside the realm create a client_id: `hapifhir-client`
* Create a user: test. Modify the creadentials (temporary off)
4. Access the authenticator `http://localhost:8081/swagger-ui.html`
* Open Login operation under Auth controller, `Try it out`:
* In the JSON include the user created in Keycloak (`test`)
* The response is a OAuth access token
* Copy the access token
* Last thing is to configure the [HAPI client including the authorization token in the header](http://hapifhir.io/doc_rest_client_interceptor.html). Authorization header: `Bearer <access token>` (see the java snippet below)
```
		BearerTokenAuthInterceptor authInterceptor = new BearerTokenAuthInterceptor(token); 
		// Create a client and post the transaction to the server
		IGenericClient client = ctx.newRestfulGenericClient(FHIR_URL);
		// Register the interceptor with your client (either style)
		client.registerInterceptor(authInterceptor);
```
5. Check that the resource has been created in HAPI `http://localhost:8080`

### Simple deployment

It is supposed [auth](https://hub.docker.com/r/ccavero/keycloak-auth) are deployed (check the docker-compose deployment for the full stack).

#### Database

Deploy a MySQL instance:
```
	docker run -d --name mysql -e MYSQL_ROOT_PASSWORD=rootpwd -e MYSQL_USER=fhiruser -e MYSQL_PASSWORD=fhirpwd MYSQL_DATABASE=fhirdb mysql:5.7
```

Deploy a MariaDB instance:
```
	docker run -d --name mariadb -e MYSQL_ROOT_PASSWORD=rootpwd -e MYSQL_USER=fhiruser -e MYSQL_PASSWORD=fhirpwd MYSQL_DATABASE=fhirdb mariadb/server:10.3
```

Deploy a PostgreSQL instance:
```
	docker run -d --name postgres -e POSTGRES_USER=fhiruser -e POSTGRES_PASSWORD=fhirpwd POSTGRES_DB=fhirdb postgres
```

#### HAPI CDR

Build the image:
```
	docker build -t hapi-fhir/hapi-fhir-cdr .
```

Use this command to start the container (take into account the links to auth and database containers). The possibilities for the DB_VENDOR are [DERBY, MYSQL, MARIADB, POSTGRESQL (DB_PORT 5432)]: 
```
	docker run -d --name hapi-fhir-cdr -p 8080:8080 hapi-fhir/hapi-fhir-cdr -e DB_VENDOR=MARIADB -e DB_HOST=mariadb -e DB_PORT=3306 -e DB_USER=fhiruser -e DB_PASSWORD=fhirpwd DB_DATABASE=fhirdb -e LUCENE_FOLDER=XXX OAUTH_ENABLE=true OAUTH_URL=http://auth:8081/ --link auth:auth --link mariadb:mariadb 
```

Note: with this command data is persisted across container restarts, but not after removal of the container. Use a new container for the database with a shared docker volume.

## MySQL configuration

We follow the recommended [MySQL configuration](https://groups.google.com/forum/#!topic/hapi-fhir/ValHrT3hAj0) including extra jpaProperties to [avoid permission problems with Lucene indexes](https://groups.google.com/forum/#!topic/hapi-fhir/wyh4TEpUuSA) of the default configuration:

1. Dependency in the pom:
```
    <dependency>
        <groupId>mysql</groupId>
        <artifactId>mysql-connector-java</artifactId>
        <version>6.0.5</version>
    </dependency>
```
2. In FhirServerConfig

* In the data source
  
```
    public DataSource dataSource() {
      BasicDataSource retVal = new BasicDataSource();
        try {
            retVal.setDriver(new com.mysql.cj.jdbc.Driver());
        } catch (SQLException e) {
        
```

* In the JPA properties

```
    private Properties jpaProperties() {

      Properties extraProperties = new Properties();

      // Use MySQL hibernate
      extraProperties.put("hibernate.dialect", "org.hibernate.dialect.MySQL5InnoDBDialect");
	  
      // To avoid problems with Lucene indexes permissions
      extraProperties.put("hibernate.search.default.indexBase", "/var/lib/tomcat8/webapps/hapi-fhir-jpaserver-example-mysql-oauth/indexes");
    }
```

## OAuth2 authorization

We use as IdM [KeyCloak](http://www.keycloak.org/). [OAuth2 authorization in HAPI](http://hapifhir.io/doc_rest_server_security.html#Authorization_Interceptor) is done [via Interceptors](http://hapifhir.io/doc_rest_server_interceptor.html). We reuse the [careconnect implementation](https://github.com/nhsconnect/careconnect-reference-implementation/blob/master/ccri-fhirgatewayhttps/src/main/java/uk/nhs/careconnect/ri/gateway/https/oauth2/OAuthTokenUtil.java) creating a new IServerInterceptor in FhirConfig that is automatically registered when launching the server:
```
    @Bean(autowire = Autowire.BY_TYPE)
    public IServerInterceptor subscriptionKeyCloakInterceptor() {
       KeyCloakInterceptor retVal = new KeyCloakInterceptor();
       return retVal;
    }
```


```
    BearerTokenAuthInterceptor authInterceptor = new BearerTokenAuthInterceptor(token);

    // Create a client and post the transaction to the server
    IGenericClient client = ctx.newRestfulGenericClient(FHIR_URL);
    // Register the interceptor with your client (either style)
    client.registerInterceptor(authInterceptor);
```


## License

Apache 2.0

By downloading this software, the downloader agrees with the specified terms and conditions of the License Agreement and the particularities of the license provided.