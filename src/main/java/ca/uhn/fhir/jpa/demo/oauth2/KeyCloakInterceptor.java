/*
 * Copyright (C) 2018  Atos Spain SA. All rights reserved.
 * 
 * This file is part of the hapi-fhir-jpaserver-example-mysql-oauth.
 * 
 * KeyCloakInterceptor.java is free software: you can redistribute it and/or modify it under the 
 * terms of the Apache License, Version 2.0 (the License);
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * The software is provided "AS IS", without any warranty of any kind, express or implied,
 * including but not limited to the warranties of merchantability, fitness for a particular
 * purpose and noninfringement, in no event shall the authors or copyright holders be 
 * liable for any claim, damages or other liability, whether in action of contract, tort or
 * otherwise, arising from, out of or in connection with the software or the use or other
 * dealings in the software.
 * 
 * See README file for the full disclaimer information and LICENSE file for full license 
 * information in the project root.
 * 
 * @author	Carlos Cavero
 *			Atos Research and Innovation, Atos SPAIN SA
 * 
 * Interceptor which checks the authenticity of the KeyCloak token in the header
 */

package ca.uhn.fhir.jpa.demo.oauth2;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import ca.uhn.fhir.rest.server.exceptions.AuthenticationException;
import ca.uhn.fhir.rest.server.interceptor.InterceptorAdapter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

public class KeyCloakInterceptor extends InterceptorAdapter {
	private static final Logger logger = LoggerFactory.getLogger(KeyCloakInterceptor.class);
	
	// Const from properties
	private String OAUTH_ENABLE;
	private String OAUTH_URL;
	private String BEARER = "BEARER ";

	@Override
    public boolean incomingRequestPreProcessed(HttpServletRequest theRequest, HttpServletResponse theResponse) {

        String resourcePath = theRequest.getPathInfo();
        logger.info("Accessing Resource" + resourcePath);
        /* if (excludedPaths.contains(resourcePath)){
            logger.info("Accessing unprotected resource" + resourcePath);
            return true;
        } */

		/**
		 * Read the environment variables 
		 */
		
		OAUTH_ENABLE =  System.getenv("OAUTH_ENABLE");
		OAUTH_URL =  System.getenv("OAUTH_URL");
		
		if (OAUTH_ENABLE == null) 
			OAUTH_ENABLE = "false";

		if (OAUTH_URL == null)
			OAUTH_URL = "http://localhost:8081/user";
		
		// To easily enable/disable OAuth authentication
        if (Boolean.valueOf(OAUTH_ENABLE) == false)
        	return true;
        
		String authHeader = theRequest.getHeader(HttpHeaders.AUTHORIZATION);
        if (authHeader == null){
            logger.warn("OAuth2 Authentication failure.  No OAuth Token supplied in Authorization Header on Request.");
            throw new AuthenticationException("Unauthorised access to protected resource");
        }
        
        String authToken = null;
        if (authHeader.toUpperCase().startsWith(BEARER)) 
            authToken = authHeader.substring(BEARER.length());
        else 
            throw new AuthenticationException("Invalid OAuth Header. Missing Bearer prefix");
        
        RestTemplate restTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();
        headers.set("Authorization", authToken);

        HttpEntity<String> entity = new HttpEntity<String>(headers);

        ResponseEntity<String> response = restTemplate.exchange(
        		OAUTH_URL, HttpMethod.GET, entity, String.class);
        // AccessToken oauthToken = response.getBody();        
        
   		if (response.getStatusCode().value() != 200) {
            logger.warn("OAuth2 Authentication failure. Invalid OAuth Token supplied in Authorization Header on Request.");
            throw new AuthenticationException("Unauthorised access to protected resource");
        }

        // Check that the OAuth Token has not expired
        /* if (oAuthToken.isExpired()){
            logger.warn("OAuth2 Authentication failure due to expired token");
            throw new AuthenticationException("OAuth2 Authentication Token has expired.");
        } */

        // Check that the Scopes on the Token allow access to the specified resource
        /* String resourceName = extractResourceName(resourcePath);
        if (!allowedAccess(resourceName, theRequest.getMethod(), oAuthToken)){
            logger.warn("OAuth2 Authentication failed due to insufficient access rights: ");
            throw new ForbiddenOperationException(String.format("Insufficient Access Rights to access %s.", resourceName));
        } */

        logger.debug("Authenticated Access to " + resourcePath);
        return true;
    }
}
