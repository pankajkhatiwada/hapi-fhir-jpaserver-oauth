package ca.uhn.fhir.jpa.demo;

import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.Properties;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import ca.uhn.fhir.jpa.search.LuceneSearchMappingFactory;
import org.apache.commons.dbcp2.BasicDataSource;
import org.apache.commons.lang3.time.DateUtils;
import org.hibernate.jpa.HibernatePersistenceProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowire;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import ca.uhn.fhir.jpa.config.BaseJavaConfigDstu3;
import ca.uhn.fhir.jpa.dao.DaoConfig;
import ca.uhn.fhir.jpa.demo.oauth2.KeyCloakInterceptor;
import ca.uhn.fhir.jpa.util.SubscriptionsRequireManualActivationInterceptorDstu3;
import ca.uhn.fhir.rest.server.interceptor.IServerInterceptor;
import ca.uhn.fhir.rest.server.interceptor.LoggingInterceptor;
import ca.uhn.fhir.rest.server.interceptor.ResponseHighlighterInterceptor;

/**
 * This is the primary configuration file for the example server
 */
@Configuration
@EnableTransactionManagement()
public class FhirServerConfig extends BaseJavaConfigDstu3 {
	private static final Logger LOGGER = LoggerFactory.getLogger(FhirServerConfig.class);

	private final static String CONFIGURATIONFILE = "configuration.properties";

	// Const from properties
	private String MYSQL_URL;
	private String MYSQL_PORT;
	private String MYSQL_DB;
	private String MYSQL_USER;
	private String MYSQL_PASS;
	private String OAUTH;

	/**
	 * Configure FHIR properties around the the JPA server via this bean
	 */
	@Bean()
	public DaoConfig daoConfig() {
		DaoConfig retVal = new DaoConfig();
		retVal.setAllowMultipleDelete(true);
		return retVal;
	}

	/**
	 * The following bean configures the database connection. The 'url' property value of "jdbc:derby:directory:jpaserver_derby_files;create=true" indicates that the server should save resources in a
	 * directory called "jpaserver_derby_files".
	 * 
	 * A URL to a remote database could also be placed here, along with login credentials and other properties supported by BasicDataSource.
	 */
	@Bean(destroyMethod = "close")
	public DataSource dataSource() {
		/* BasicDataSource retVal = new BasicDataSource();
		retVal.setDriver(new org.apache.derby.jdbc.EmbeddedDriver());
		retVal.setUrl("jdbc:derby:directory:target/jpaserver_derby_files;create=true");
		retVal.setUsername("");
		retVal.setPassword("");
		return retVal; */

		/**
		 * Read the properties file. It should be located in the resources folder 
		 * when generating the war file. 
		 */
		
		try {
			Properties prop = new Properties();
			InputStream inputStream = getClass().getClassLoader().getResourceAsStream(CONFIGURATIONFILE);
			prop.load(inputStream);
			MYSQL_URL = prop.getProperty("mysqlUrl");
			MYSQL_PORT = prop.getProperty("mysqlPort");
			MYSQL_DB = prop.getProperty("mysqlDb");
			MYSQL_USER = prop.getProperty("mysqlUser");
			MYSQL_PASS = prop.getProperty("mysqlPass");
			OAUTH = prop.getProperty("OAuth");
			LOGGER.debug("MYSQL URL: " + MYSQL_URL + ":" + MYSQL_PORT);
		} catch (IOException e) {
			LOGGER.error("Configuration file not found", e);
		}
		
		BasicDataSource retVal = new BasicDataSource();
		
		try {
			retVal.setDriver(new com.mysql.jdbc.Driver());
		} catch (SQLException e) {
			LOGGER.error("MySQL driver not properly loaded", e);
		}
		retVal.setUrl("jdbc:mysql://" +
				MYSQL_URL + ":" +
				MYSQL_PORT + "/" + MYSQL_DB + 
				"?useSSL=false&serverTimezone=UTC");
		
		retVal.setUsername(MYSQL_USER);
		retVal.setPassword(MYSQL_PASS);
		return retVal;
	}

	@Bean()
	public LocalContainerEntityManagerFactoryBean entityManagerFactory() {
		LocalContainerEntityManagerFactoryBean retVal = new LocalContainerEntityManagerFactoryBean();
		retVal.setPersistenceUnitName("HAPI_PU");
		retVal.setDataSource(dataSource());
		retVal.setPackagesToScan("ca.uhn.fhir.jpa.entity");
		retVal.setPersistenceProvider(new HibernatePersistenceProvider());
		retVal.setJpaProperties(jpaProperties());
		return retVal;
	}

	private Properties jpaProperties() {
		Properties extraProperties = new Properties();
		extraProperties.put("hibernate.dialect", "org.hibernate.dialect.MySQL5InnoDBDialect");
		//extraProperties.put("hibernate.dialect", org.hibernate.dialect.DerbyTenSevenDialect.class.getName());
		extraProperties.put("hibernate.format_sql", "true");
		extraProperties.put("hibernate.show_sql", "false");
		extraProperties.put("hibernate.hbm2ddl.auto", "update");
		extraProperties.put("hibernate.jdbc.batch_size", "20");
		extraProperties.put("hibernate.cache.use_query_cache", "false");
		extraProperties.put("hibernate.cache.use_second_level_cache", "false");
		extraProperties.put("hibernate.cache.use_structured_entries", "false");
		extraProperties.put("hibernate.cache.use_minimal_puts", "false");
		extraProperties.put("hibernate.search.model_mapping", LuceneSearchMappingFactory.class.getName());
		extraProperties.put("hibernate.search.default.directory_provider", "filesystem");
		extraProperties.put("hibernate.search.default.indexBase", "/var/lib/tomcat8/webapps/hapi/indexes");
//		extraProperties.put("hibernate.search.default.indexBase", "target/lucenefiles");
		extraProperties.put("hibernate.search.lucene_version", "LUCENE_CURRENT");
//		extraProperties.put("hibernate.search.default.worker.execution", "async");
		return extraProperties;
	}

	/**
	 * Do some fancy logging to create a nice access log that has details about each incoming request.
	 */
	public IServerInterceptor loggingInterceptor() {
		LoggingInterceptor retVal = new LoggingInterceptor();
		retVal.setLoggerName("fhirtest.access");
		retVal.setMessageFormat(
				"Path[${servletPath}] Source[${requestHeader.x-forwarded-for}] Operation[${operationType} ${operationName} ${idOrResourceName}] UA[${requestHeader.user-agent}] Params[${requestParameters}] ResponseEncoding[${responseEncodingNoDefault}]");
		retVal.setLogExceptions(true);
		retVal.setErrorMessageFormat("ERROR - ${requestVerb} ${requestUrl}");
		return retVal;
	}

	/**
	 * This interceptor adds some pretty syntax highlighting in responses when a browser is detected
	 */
	@Bean(autowire = Autowire.BY_TYPE)
	public IServerInterceptor responseHighlighterInterceptor() {
		ResponseHighlighterInterceptor retVal = new ResponseHighlighterInterceptor();
		return retVal;
	}

	@Bean(autowire = Autowire.BY_TYPE)
	public IServerInterceptor subscriptionSecurityInterceptor() {
		SubscriptionsRequireManualActivationInterceptorDstu3 retVal = new SubscriptionsRequireManualActivationInterceptorDstu3();
		return retVal;
	}

	@Bean(autowire = Autowire.BY_TYPE)
	public IServerInterceptor subscriptionKeyCloakInterceptor() {
		KeyCloakInterceptor retVal = new KeyCloakInterceptor();
		return retVal;
	}

	@Bean()
	public JpaTransactionManager transactionManager(EntityManagerFactory entityManagerFactory) {
		JpaTransactionManager retVal = new JpaTransactionManager();
		retVal.setEntityManagerFactory(entityManagerFactory);
		return retVal;
	}
	
}
