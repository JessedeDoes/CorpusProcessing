package bab_conversie;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;


public class TeiBuilder {
	
	boolean paragraphStarted;
	boolean sentenceStarted;
	StringBuffer teiDoc;
	
	String openingDelTag = "&lt;del&gt;";
	String closingDelTag = "&lt;/del&gt;";
	
	boolean useAlternativeTags = false;
	
	public TeiBuilder(){
		paragraphStarted = false;
		sentenceStarted = false;
	}
	
	private void addFeaturesIfPresent(String nForm, String originalToken){
		
		if ( !originalToken.isEmpty())
		{
			String encodedOriginalToken = originalToken;
//			try {
//				encodedOriginalToken = java.net.URLEncoder.encode(originalToken, "UTF-8");
//			} catch (UnsupportedEncodingException e) {
//				throw new RuntimeException(e);
//			}
			teiDoc.append(" ");
			teiDoc.append("original=");
			teiDoc.append("\""+encodedOriginalToken+"\"");
		}		
		
		if ( !nForm.isEmpty())
		{
			teiDoc.append(" ");
			teiDoc.append("nform=");
			teiDoc.append("\""+nForm+"\"");
		}
	}
	
	
	private void openTagsThatMustBeOpenAtThatPoint(){
		// if paragraph is not open yet, open it
		if ( !paragraphStarted)
		{
			if (useAlternativeTags) teiDoc.append("\t<ab>\n");
			paragraphStarted = true;
		}
		// if sentence is not open yet, open it
		if ( !sentenceStarted)
		{
			if (useAlternativeTags) teiDoc.append("\t<s>\n");
			sentenceStarted = true;					
		}
	}

	private void closeTagsThatMustBeClosedAtThatPoint(){
		// if sentence is not closed yet (because no dots was found at the end)
		// close the sentence first!
		if (sentenceStarted)
		{
			if (useAlternativeTags) teiDoc.append("\t</s>\n");
			sentenceStarted = false;
		}
		// close the paragraph
		// (check is paragraph was closed already to prevent double closing tag)
		if (paragraphStarted)
		{
			if (useAlternativeTags) teiDoc.append("\t</ab>\n");
			paragraphStarted = false;
		}
		
	}
	
	private void addSpaceIfInSentence(){
		if (sentenceStarted)
			teiDoc.append("\t<c> </c>\n");
	}
	
	public String buildDoc(ArrayList<String[]> tokens, 
			String fileName, String filenameWithoutPath, String headerInfo)
	{
		teiDoc = new StringBuffer();
		
		teiDoc.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
		teiDoc.append("<TEI.2>\n");
		teiDoc.append("<teiHeader type=\"letter\">");
		if ( !headerInfo.isEmpty())
		{
			teiDoc.append("\n\t<fileDesc>");
			teiDoc.append(headerInfo.trim());
			teiDoc.append("</fileDesc>\n");
		}
		teiDoc.append("</teiHeader>\n");
		teiDoc.append("<text>\n");
		teiDoc.append("<body>\n");
		
		teiDoc.append("<div type=\"letter\" " +
				"xmlns=\"http://www.tei-c.org/ns/1.0\" xml:lang=\"nl\" n=\""+
				filenameWithoutPath.replaceAll("(\\.\\w{3,4})$", "")+"\">\n");
		teiDoc.append("<p>\n");
		
		int emptyLineCounter = 0;
		boolean deleteMode = false; // is set to true when we are inside a <del> tag
		
		for (String[] tokenArray : tokens)
		{
			String bareToken = tokenArray[0];
			String originalToken = tokenArray[1];
			String nForm         = tokenArray[2];
			
			// token is empty line marker
			// so we must close the paragraph
			if ( bareToken.equals(Constants.EMPTY_LINE_MARKER))
			{
				emptyLineCounter++;
				closeTagsThatMustBeClosedAtThatPoint();
			}
			else
			{
				// we have something else than an empty line marker
				// first register the empty line markers we encountered
				if ( !useAlternativeTags && emptyLineCounter>0)
				{
					if (emptyLineCounter==1)
						teiDoc.append("\t<lb/>\n");					
					else
						teiDoc.append("\t<lb n=\""+emptyLineCounter+"\"/>\n");	
				}
				// reset the empty line counter
				// before carrying on
				emptyLineCounter= 0; 
				
				
				// we are finished with handling the empty line markers we
				// encountered, now carry on with current content
				
				// token is punctuation (other than a dot)
				if ( bareToken.matches("^([^\\.%&0-9a-zA-Z��������������������������������������������])$"))
				{
					addSpaceIfInSentence();
					openTagsThatMustBeOpenAtThatPoint();
					
					teiDoc.append("\t<pc ctag=\""+bareToken+"\"");
					addFeaturesIfPresent(nForm, originalToken);
					teiDoc.append(">");
					teiDoc.append(bareToken);
					teiDoc.append("</pc>\n");
				}
				// token is unreadable part (marked als [...])
				else if ( bareToken.matches("^(\\[\\.{1,3}])$"))
				{
					addSpaceIfInSentence();
					openTagsThatMustBeOpenAtThatPoint();
					
					teiDoc.append("\t<pc ctag=\""+bareToken+"\"");
					addFeaturesIfPresent(nForm, originalToken);
					teiDoc.append(">");
					teiDoc.append(bareToken);
					teiDoc.append("</pc>\n");
				}
				// token is end of sentence mark
				// so we must close the sentence
				else if (bareToken.matches("^(\\.|\\?|!)$"))
				{
					addSpaceIfInSentence();
					openTagsThatMustBeOpenAtThatPoint();

					teiDoc.append("\t<pc ctag=\""+bareToken+"\"");
					addFeaturesIfPresent(nForm, originalToken);
					teiDoc.append(">");
					teiDoc.append(bareToken);
					teiDoc.append("</pc>\n");
					
					// close sentence
					if (sentenceStarted)
					{
						if (useAlternativeTags) teiDoc.append("\t</s>\n");
						sentenceStarted = false;
					}
				}
				// token is a word 
				else 
				{
					addSpaceIfInSentence();
					openTagsThatMustBeOpenAtThatPoint();
					
					// is it a deleted word?
					if ( 	(  originalToken.startsWith(openingDelTag) && !originalToken.contains(closingDelTag) ) 
							|| 
							( !originalToken.contains(openingDelTag) &&    originalToken.endsWith(closingDelTag) ) 
							|| 
							( originalToken.startsWith(openingDelTag) &&   originalToken.endsWith(closingDelTag))
							||
							deleteMode)
					{
						teiDoc.append("\t<del rend=\"overstrike\">"+bareToken+"</del>\n");
						
						if (originalToken.startsWith(openingDelTag)) deleteMode =  true;
						// no 'if else' so as to allow 'deleteMode=false', 
						// even when opening <del> was also present;
						// because the closing </del> must be able to force 'false' back
						if (originalToken.endsWith(closingDelTag)) deleteMode = false;
					}
					// is it a partly deleted word? [ <del>s</del>voert ]
					else if (originalToken.contains(openingDelTag) && originalToken.contains(closingDelTag))
					{
						String wordWithoutDeletedPart = originalToken.replaceAll("(&lt;del.*?del&gt;)", "");
						String wordWithoutOtherTags = wordWithoutDeletedPart.replaceAll("(&lt;)(.*?)(&gt;)", "");
						teiDoc.append("\t<w");
						addFeaturesIfPresent( "", originalToken);
						teiDoc.append(">");
						teiDoc.append(wordWithoutOtherTags);
						teiDoc.append("</w>\n");
					}
					// add word token
					else
					{						
						teiDoc.append("\t<w");
						addFeaturesIfPresent(nForm, originalToken);
						teiDoc.append(">");
						teiDoc.append(bareToken);
						teiDoc.append("</w>\n");
					}
				}
				
				
				
			}
						
			
		}
		
		// we are finished with all tokens
		
		closeTagsThatMustBeClosedAtThatPoint();
		
		teiDoc.append("</p>\n");
		teiDoc.append("</div>\n");
		teiDoc.append("</body>\n");
		teiDoc.append("</text>\n");		
		teiDoc.append("</TEI.2>");
		
		return teiDoc.toString();
	}
	
	
	public void writeToUtf8File(String content, String targetFolder, String filename) {
        
		if ( !filename.endsWith(".xml"))
		{
			//filename = filename.replaceAll("(\\.\\w{3,4})$", "");
			filename = filename.replaceAll(".[tT][xX][tT]$", "");
			filename += ".xml";
		}
			
        filename = filename.replaceAll("[^A-Za-z0-9_.-]", "_");

    	FileOutputStream fos = null;
    	OutputStreamWriter osw = null;
        BufferedWriter bw = null;

        System.err.println("saving to " + filename + " in directory " + targetFolder);
        
        try {
            
            //Construct the BufferedWriter object
        	fos = new FileOutputStream(targetFolder + "/" + filename);
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
