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

If you enable the OAuth capability deploy the [Keycloak OAuth 2.0](https://github.com/AriHealth/keycloak-auth) and dependencies.

## Environment variables

Some environment variables must be set prior to the execution:

	DB_VENDOR=
	MYSQL_URL=
	MYSQL_USER=
	MYSQL_PASS=
	LUCENE_FOLDER=/var/lib/tomcat8/webapps/hapi/indexes
	OAUTH_ENABLE=true or false
	OAUTH_URL=

## Docker deployment

Build the image:
```
	docker build -t hapi-fhir/hapi-fhir-cdr .
```

Use this command to start the container: 
```
	docker run -d --name hapi-fhir-cdr -p 8080:8080 hapi-fhir/hapi-fhir-cdr -e DB_VENDOR=MYSQL -e MYSQL_URL=XXX -e MYSQL_USER=XXX -e MYSQL_PASS=XXX -e LUCENE_FOLDER=XXX
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

[OAuth2 authorization in HAPI](http://hapifhir.io/doc_rest_server_security.html#Authorization_Interceptor) is done [via Interceptors](http://hapifhir.io/doc_rest_server_interceptor.html). We reuse the [careconnect implementation](https://github.com/nhsconnect/careconnect-reference-implementation/blob/master/ccri-fhirgatewayhttps/src/main/java/uk/nhs/careconnect/ri/gateway/https/oauth2/OAuthTokenUtil.java) creating a new IServerInterceptor in FhirConfig that is automatically registered when launching the server:
```
    @Bean(autowire = Autowire.BY_TYPE)
    public IServerInterceptor subscriptionKeyCloakInterceptor() {
       KeyCloakInterceptor retVal = new KeyCloakInterceptor();
       return retVal;
    }
```

We use as IdM [KeyCloak](http://www.keycloak.org/). Provision scripts to run [KeyCloak and HAPI behind a reverse proxy](https://github.com/Codingpedia/codingmarks-api/wiki/Keycloak-Setup-for-Production) are provided [here](https://github.com/AriHealth/puppet-ari). The REST API which provides login and isValid authorization are also provided [here](https://github.com/AriHealth/keycloak-auth). Last thing is to configure the [HAPI client including the authorization token in the header](http://hapifhir.io/doc_rest_client_interceptor.html):

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