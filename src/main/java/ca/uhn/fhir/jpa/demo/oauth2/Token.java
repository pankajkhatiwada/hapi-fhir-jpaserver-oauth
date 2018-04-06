package ca.uhn.fhir.jpa.demo.oauth2;


public class Token {
	private boolean valid;
	private String info;

	public Token () {
	}
	
	public boolean getValid() {
		return valid;
	}

	public void setValid(boolean valid) {
		this.valid = valid;
	}

	public String getInfo() {
		return info;
	}

	public void setInfo(String info) {
		this.info = info;
	}

}
