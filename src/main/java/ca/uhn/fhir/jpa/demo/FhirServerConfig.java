package ca.uhn.fhir.jpa.demo;

import java.sql.SQLException;
import java.util.Properties;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import ca.uhn.fhir.jpa.search.LuceneSearchMappingFactory;
import org.apache.commons.dbcp2.BasicDataSource;
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

	// Const from properties
	private static final String DB_VENDOR = System.getenv("DB_VENDOR");
	private static final String MYSQL_URL = System.getenv("MYSQL_URL");
	private static final String MYSQL_USER = System.getenv("MYSQL_USER");
	private static final String MYSQL_PASS = System.getenv("MYSQL_PASS");
	private static final String LUCENE_FOLDER = System.getenv("LUCENE_FOLDER");	
	
	private static final String DEFAULT_LUCENE_FOLDER = "target/lucenefiles";
	private static final String MYSQL_DB_VENDOR = "MYSQL";
	private static final String DERBY_DB_VENDOR = "DERBY";

	

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
	 * The following bean configures the database connection. 
	 * The 'url' property value of "jdbc:derby:directory:jpaserver_derby_files;create=true"
	 * indicates that the server should save resources in a directory called "jpaserver_derby_files".
	 * 
	 * A URL to a remote database could also be placed here, along with login credentials and other properties supported by BasicDataSource.
	 * @throws SQLException 
	 */
	@Bean(destroyMethod = "close")
	public DataSource dataSource() {
		try(BasicDataSource retVal = new BasicDataSource()) {
			switch (DB_VENDOR) {
				case MYSQL_DB_VENDOR:
					retVal.setDriver(new com.mysql.jdbc.Driver());
					retVal.setUrl(MYSQL_URL);
					retVal.setUsername(MYSQL_USER);
					retVal.setPassword(MYSQL_PASS);
					break;
				case DERBY_DB_VENDOR:
				default:
					retVal.setDriver(new org.apache.derby.jdbc.EmbeddedDriver());
					retVal.setUrl("jdbc:derby:directory:target/jpaserver_derby_files;create=true");
					retVal.setUsername("");
					retVal.setPassword("");
			}
			return retVal;
		} catch (SQLException sqlex) {
			LOGGER.error("Exception in database connection", sqlex);
			return null;
		}
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
		LOGGER.info("DB_VENDOR: {}", DB_VENDOR);

		switch (DB_VENDOR) {
			case MYSQL_DB_VENDOR:
				extraProperties.put("hibernate.dialect", "org.hibernate.dialect.MySQL5InnoDBDialect");
				break;
			case DERBY_DB_VENDOR:
			default:
				extraProperties.put("hibernate.dialect", org.hibernate.dialect.DerbyTenSevenDialect.class.getName());
		}
		extraProperties.put("hibernate.format_sql", Boolean.TRUE.toString());
		extraProperties.put("hibernate.show_sql", Boolean.FALSE.toString());
		extraProperties.put("hibernate.hbm2ddl.auto", "update");
		extraProperties.put("hibernate.jdbc.batch_size", "20");
		extraProperties.put("hibernate.cache.use_query_cache", Boolean.FALSE.toString());
		extraProperties.put("hibernate.cache.use_second_level_cache", Boolean.FALSE.toString());
		extraProperties.put("hibernate.cache.use_structured_entries", Boolean.FALSE.toString());
		extraProperties.put("hibernate.cache.use_minimal_puts", Boolean.FALSE.toString());
		extraProperties.put("hibernate.search.model_mapping", LuceneSearchMappingFactory.class.getName());
		extraProperties.put("hibernate.search.default.directory_provider", "filesystem");
		extraProperties.put("hibernate.search.default.indexBase", LUCENE_FOLDER == null? DEFAULT_LUCENE_FOLDER:LUCENE_FOLDER);
		extraProperties.put("hibernate.search.lucene_version", "LUCENE_CURRENT");
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
		return new ResponseHighlighterInterceptor();
	}

	@Bean(autowire = Autowire.BY_TYPE)
	public IServerInterceptor subscriptionSecurityInterceptor() {
		return new SubscriptionsRequireManualActivationInterceptorDstu3();
	}

	@Bean(autowire = Autowire.BY_TYPE)
	public IServerInterceptor subscriptionKeyCloakInterceptor() {
		return new KeyCloakInterceptor();
	}

	@Bean()
	public JpaTransactionManager transactionManager(EntityManagerFactory entityManagerFactory) {
		JpaTransactionManager retVal = new JpaTransactionManager();
		retVal.setEntityManagerFactory(entityManagerFactory);
		return retVal;
	}
	
}
