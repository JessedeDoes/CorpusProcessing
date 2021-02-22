package bab_conversie;
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;


public class DotWordsLogger {

	StringBuilder sb = new StringBuilder();
	int maxLength;
	ArrayList<String> knownAbbreviations;
	String knownAbbreviationsStr = "";
	
	public DotWordsLogger(int maxLength){
		
		this.sb = new StringBuilder();
		this.maxLength = maxLength;
		
		// put known abbreviations once in a hashmap, to speed up the process
		knownAbbreviations = new ArrayList<String>();
		
		for (int i =0; i< Constants.KNOWN_ABBREVIATIONS.length; i++)
		{
			if ( !knownAbbreviationsStr.isEmpty()) knownAbbreviationsStr+="|";
			knownAbbreviationsStr += Constants.KNOWN_ABBREVIATIONS[i];
			//knownAbbreviations.add(Constants.KNOWN_ABBREVIATIONS[i]);
		}
		knownAbbreviationsStr = "^("+knownAbbreviationsStr+")$";
		
	}
	
	public void buildLog(String fileName, ArrayList<String[]> tokens){		
		
		String top = "\n\n\n\n"+fileName+"\n"+
				"-------------------------------------------".substring(fileName.length())+
				"\n";	
		boolean topAdded = false;
		ArrayList<String> tokensAlreadyAdded = new ArrayList<String>();
		
		for (String[] oneTokenSet : tokens){
			
			String bareToken = oneTokenSet[0];
			
			if (tokensAlreadyAdded.contains(bareToken) || bareToken.length() > maxLength )
				continue;
			
			if (( bareToken.contains(".") || bareToken.contains(":") ) )
			{
				boolean isKnownAbbreviation = false;
				if (bareToken.matches(knownAbbreviationsStr) )//knownAbbreviations.contains(bareToken))
					isKnownAbbreviation = true;
				else if (bareToken.matches("(.*)([^-0-9a-zA-Z��������������������������������������������])([A-Za-z])(\\.|:)(.*)"))
					isKnownAbbreviation = true;
				else if (bareToken.matches("^([A-Za-z])(\\.|:)(.*)"))
					isKnownAbbreviation = true;
				else if (bareToken.contains("[..") || bareToken.contains("..]") )
					isKnownAbbreviation = true;
				
				if ( !isKnownAbbreviation )
				{
					if ( !topAdded) {this.sb.append(top); topAdded=true;}
					this.sb.append(bareToken+"\n");
					tokensAlreadyAdded.add(bareToken);
				}
				
			}
		}
		
		
	}
	
	public void writeLog(String mapAndFileName){
		
		writeToUtf8File(this.sb.toString(), mapAndFileName);
		
	}
	
	
	
	private void writeToUtf8File(String content, String filename) {
        
		if ( !filename.endsWith(".log"))
		{
			filename = filename.replaceAll("(\\.\\w{3,4})$", "");
			filename += ".log";
			
		}
			
		
    	FileOutputStream fos = null;
    	OutputStreamWriter osw = null;
        BufferedWriter bw = null;
        
        
        try {
            
            //Construct the BufferedWriter object
        	fos = new FileOutputStream(filename);
        	osw = new OutputStreamWriter(fos, "UTF8");
            bw = new BufferedWriter(osw);
            
            //Start writing to the output stream
            bw.write(content);
            
        } catch (Exception e) {
        	throw new RuntimeException(e);
        } finally {
            //Close the BufferedWriter
            try {
                if (bw != null) {
                    bw.flush();
                    bw.close();
                    osw.close();
                    fos.close();
                }
            } catch (IOException e) {
            	throw new RuntimeException(e);
            }
        }
    }
}
