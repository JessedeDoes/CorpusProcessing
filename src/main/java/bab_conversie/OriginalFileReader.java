package bab_conversie;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;


public class OriginalFileReader {
	
	
	
	
	// constructor
	public OriginalFileReader(){}
	
	
	
	// read the original files in its original encoding
	// and return the content as a string
	public String[] readFile(String filename) throws IOException{
		
		
		// prevents starting by accident!!!
		//System.exit(1);
		
		String headerInfo = "<sourceFile>" + filename + "</sourceFile>\n";
		
		File file = new File(filename);
		InputStream fis = null;
		InputStreamReader isr = null;
		BufferedReader br = null;
		
		try
		{
			fis = new FileInputStream(file);
			isr = new InputStreamReader(fis, "windows-1250");
			br = new BufferedReader(isr);
		}
		catch (Exception e) {
			throw new RuntimeException(e);
			}
		
			
		String fileContent = "";
		String line = "";
		
		
		try
		{
			while ( (line = br.readLine()) != null )
			{
				//System.out.println("line "+line.trim().isEmpty()+" [[["+line+"]]]");
				if (line.trim().isEmpty())
					line = " "+Constants.EMPTY_LINE_MARKER+" ";
				
				if (fileContent.isEmpty() &&  line.trim().matches("^\\s*(\\[.*])\\s*$"))
					headerInfo += "<image>" + line.replaceAll("^\\s*\\[|\\]\\s*$","")  + "</image>\n";
				else
				{
					fileContent += line + " " + Constants.EMPTY_LINE_MARKER  + " ";
				}
			}
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
		
		try {
            if (br != null) {
            	br.close();
            	isr.close();
            	fis.close();
            }
        } catch (Exception e) {
        	throw new RuntimeException(e);
        }
				
		return new String[]{headerInfo, fileContent.trim()};
	}

}
