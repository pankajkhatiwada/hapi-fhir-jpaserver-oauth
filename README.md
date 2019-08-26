# Hapi-fhir-jpaserver-oauth

## Description

Hapi-fhir-jpaserver-oauth is HAPI v 3.8.0-SNAPSHOT with support for mySQL and OAuth. 

## Environment variables

Some environment variables must be set prior to the execution:

	DB_VENDOR=
	MYSQL_URL=
	MYSQL_USER=
	MYSQL_PASS=
	LUCENE_FOLDER=/var/lib/tomcat8/webapps/hapi/indexes
	OAUTH_ENABLE=true or false
	OAUTH_URL=

## Running hapi-fhir-jpaserver-example in a Docker container

Execute the `build-docker-image.sh` script to build the docker image. 

Use this command to start the container: 
  `docker run -d --name hapi-fhir-jpaserver-oauth -p 8080:8080 hapi-fhir/hapi-fhir-jpaserver-oauth -e DB_VENDOR=MYSQL -e MYSQL_URL=XXX -e MYSQL_USER=XXX -e MYSQL_PASS=XXX -e LUCENE_FOLDER=XXX`

Note: with this command data is persisted across container restarts, but not after removal of the container. Use a docker volume mapping on /var/lib/jetty/target to achieve this.

## Running hapi-fhir-jpaserver-example in Tomcat from IntelliJ

Install Tomcat.

Make sure you have Tomcat set up in IntelliJ.

- File->Settings->Build, Execution, Deployment->Application Servers
- Click +
- Select "Tomcat Server"
- Enter the path to your tomcat deployment for both Tomcat Home (IntelliJ will fill in base directory for you)

Add a Run Configuration for running hapi-fhir-jpaserver-example under Tomcat

- Run->Edit Configurations
- Click the green +
- Select Tomcat Server, Local
- Change the name to whatever you wish
- Uncheck the "After launch" checkbox
- On the "Deployment" tab, click the green +
- Select "Artifact"
- Select "hapi-fhir-jpaserver-example:war" 
- In "Application context" type /hapi

Run the configuration.

- You should now have an "Application Servers" in the list of windows at the bottom.
- Click it.
- Select your server, and click the green triangle (or the bug if you want to debug)
- Wait for the console output to stop

Point your browser (or fiddler, or what have you) to `http://localhost:8080/hapi/base/Patient`

You should get an empty bundle back.

## Using ElasticSearch as the search engine instead of the default Apache Lucene

1. Install ElasticSearch server and the phonetic plugin
    * Download ElasticSearch from https://www.elastic.co/downloads/elasticsearch
    * ```cd {your elasticsearch directory}```
    * ```bin/plugin install analysis-phonetic```
    * start ElasticSearch server: ```./bin/elasticsearch```
2. Replace configuration in web.xml
    * replace the configuration class ```ca.uhn.fhir.jpa.demo.FhirServerConfig``` in web.xml by ```ca.uhn.fhir.jpa.demo.elasticsearch.FhirServerConfig```
3. Start server by runing: ```mvn jetty:run```
4. Limitations:
    * Hibernate search are not compatible with all ElasticSearch version. If you are using Hibernate search: 5.6 or 5.7, the compatible ElasticSearch version is 2.0 - 2.4. If you are using Hibernate search: 5.8 or 5.9, the compatible ElasticSearch version is
    2.0 - 5.6.
    * Please check all the limitations in the reference documentation: https://docs.jboss.org/hibernate/search/5.7/reference/en-US/html_single/#elasticsearch-limitations before use the integration.

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