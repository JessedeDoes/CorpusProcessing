package bab_conversie;
import java.util.ArrayList;
import java.util.HashMap;


public class Tokenizer {
	
	boolean addSpaces = false;
	
	
	// constructor
	// add spaces inbetween punctuation signs of not
	public Tokenizer(boolean addSpaces){
		this.addSpaces = addSpaces;
	}
	
	
	public ArrayList<String[]> tokenize(String content){
		
		ArrayList<String[]> tokens = new ArrayList<String[]>();
		
		String tokenizedText = content;
		
		// in a final step, we will separate the different tokens by splitting around each space
		// but before being able to do that, we must put spaces where we expect those to be 
		// able to split around those.
		
		
		// replace known abbreviations by a code
		// (so we will be able to process the other dots without
		//  possible errors since the dots of abbreviations are not
		//  visible anymore, at least temporarily)
		for (int i =0; i< Constants.KNOWN_ABBREVIATIONS.length; i++)
		{
			// build abbreviation regex
			String abbreviation = Constants.KNOWN_ABBREVIATIONS[i].replaceAll("(\\.)", "\\\\$1");
			// replace group [anything but end of a word][abbreviation][anything but begin of a word] by
			//               [anything but end of a word]<abbreviation_nr>[anything but begin of a word]
			// beware: end of a previous word attached can consist of a (double) dot, when the
			//         whole thing is actually a bigger abbreviation
			String regex = "(\\s|[^-\\.:%&a-zA-ZáéíóúýàèìòùâêîôûäëïöüñçÁÉÍÓÚÝÀÈÌÒÙÂÊÎÔÛÄËÏÖÜ])" +
					"(" + abbreviation + ")([^-%&a-zA-ZáéíóúýàèìòùâêîôûäëïöüñçÁÉÍÓÚÝÀÈÌÒÙÂÊÎÔÛÄËÏÖÜ])";


			//System.out.println(abbreviation+" "+regex);
			tokenizedText = 
				tokenizedText.replaceAll(regex, "$1@@@"+i+"@@@$3");
		}
		
		
		// replace first letters (initial) followed by a dot by first letter + something else
		tokenizedText = hideTheDotsAndDoubleDotsFromInitials(tokenizedText);
		
		// *************************************************************
		// ** NOW the dots from abbreviations and initials are hidden **
		// *************************************************************
		
		// now process the remaining dots and such
		// all punctuation now gets spaces on the right and left
		if (addSpaces)
			tokenizedText = tokenizedText.replaceAll("([^-%&0-9a-zA-ZáéíóúýàèìòùâêîôûäëïöüñçÁÉÍÓÚÝÀÈÌÒÙÂÊÎÔÛÄËÏÖÜ])"," $1 ");


		// remove double spaces and such
		tokenizedText = tokenizedText.replaceAll("\\s+", " ");
		
		
		// Build tokens array:
		// This consists of an array of array's.
		// Each subarray consists of a token, its original form and its canonical form.
		// Normally the original token is empty since the token has not been modified.
		// But when the token contains undesired tags and such, we remove those but
		// keep the original token (which is empty otherwise).
		//
		// eg:
		// a<supplied>m</supplied>ij  -> bareToken="amij" originalToken="a<supplied>m</supplied>ij"
		// meneer                     -> bareToken="meneer" originalToken=""
		
		for (String oneToken : tokenizedText.split("\\s"))
		{
			String bareToken = oneToken; 
			String originalToken = "";
			
	
			
			// if there are tags, remove all tags
			// but remember the token with tags in 'originalToken'
			if (oneToken.matches("(.*)\\<(.*?)\\>(.*)") )
			{
				bareToken = oneToken.replaceAll("(\\<)(.*?)(\\>)", "");
				originalToken = oneToken;
			}
			
			// compute a nForm (canonical form) which is the bare word without any punctuation
			// in front or at the end (word may contain tags)
			// we need this nForm only if the bare token actually has any punctuation in it
			
			// we compute it here because a that point the abbreviation dots and such
			// are hidden, so that only punctuation dots are visible
			String nForm = getNForm(bareToken);
			nForm = (nForm.equals(bareToken)) ? "" : nForm;
			
			
			// ******************************
			// ** PUT BACK the hidden dots **
			// ******************************			
			
			// replace abbreviation marks back with abbreviations.
			// we do this only now, because that way we are sure that
			// we won't wrongly remove dots (t.i. from abbreviations) when getting nforms
			if (oneToken.matches("(.*@@@)(\\d+)(@@@.*)"))
			{
				String abbrNumberStr = oneToken.replaceAll("(.*@@@)(\\d+)(@@@.*)", "$2");
				int abbrNumber = Integer.parseInt(abbrNumberStr);
				String abbreviation = Constants.KNOWN_ABBREVIATIONS[abbrNumber];
				originalToken = 
					originalToken.replaceAll("(@@@"+abbrNumber+"@@@)", abbreviation);
				bareToken = 
					bareToken.replaceAll("(@@@"+abbrNumber+"@@@)", abbreviation);
				nForm = 
					nForm.replaceAll("(@@@"+abbrNumber+"@@@)", abbreviation);				
				
			}
			
			// put first letters (initials) dots back		
			bareToken = getBackTheDotsAndDoubleDotsFromInitials(bareToken);
			originalToken = getBackTheDotsAndDoubleDotsFromInitials(originalToken);
			nForm = getBackTheDotsAndDoubleDotsFromInitials(nForm);
			
			
			
			String[] tokenArray = new String[]{
					transformIllegalSigns(bareToken), 
					transformIllegalSigns(originalToken), 
					transformIllegalSigns(nForm)
					};
			//System.out.println("token "+bareToken+"\t"+originalToken+"\t"+nForm);
			tokens.add(tokenArray);
		}
		
		
		return tokens;
	}
	
	
	private String hideTheDotsAndDoubleDotsFromInitials(String str){
		// don't put % and & in there
		str = str
			.replaceAll("(\\.)([A-Za-z])(\\.)", "EenPunt$2EenPunt")
			.replaceAll("(\\s)([A-Za-z])(\\.)", "$1$2EenPunt")
			.replaceAll("^([A-Za-z])(\\.)", "$1EenPunt");
		str = str
			.replaceAll("(:)([A-Za-z])(:)", "DubbelePunt$2DubbelePunt")
			.replaceAll("(\\s)([A-Za-z])(:)", "$1$2DubbelePunt")
			.replaceAll("^([A-Za-z])(:)", "$1DubbelePunt");
		return str;
	}
	private String getBackTheDotsAndDoubleDotsFromInitials(String str){
		// don't put % and & in there
		str = str.replaceAll("([A-Za-z])(EenPunt)", "$1.");
		//str = str.replaceAll("([^-0-9a-zA-ZáéíóúýàèìòùâêîôûäëïöüñçÁÉÍÓÚÝÀÈÌÒÙÂÊÎÔÛÄËÏÖÜ])([A-Za-z])(EenPunt)", "$1$2.");
		str = str.replaceAll("([A-Za-z])(DubbelePunt)", "$1:");
		//str = str.replaceAll("([^-0-9a-zA-ZáéíóúýàèìòùâêîôûäëïöüñçÁÉÍÓÚÝÀÈÌÒÙÂÊÎÔÛÄËÏÖÜ])([A-Za-z])(DubbelePunt)", "$1$2:");
		return str;
	}

	private String transformIllegalSigns(String str){
		return str.replaceAll("&", "&amp;")
				.replaceAll("\"", "&quot;")
				.replaceAll("\\<", "&lt;")
				.replaceAll("\\>", "&gt;");
	}

	public static String getNForm(String nForm){
		nForm = nForm.replaceAll("^([^-@<92>'<91>%&0-9a-zA-ZáéíóúýàèìòùâêîôûäëïöüñçÁÉÍÓÚÝÀÈÌÒÙÂÊÎÔÛÄËÏÖÜ]+)(.+?)$", "$2");
		nForm = nForm.replaceAll("^(.+?)([^-@<92>'<91>%&0-9a-zA-ZáéíóúýàèìòùâêîôûäëïöüñçÁÉÍÓÚÝÀÈÌÒÙÂÊÎÔÛÄËÏÖÜ]+)$", "$1");
		return nForm;
	}


}
