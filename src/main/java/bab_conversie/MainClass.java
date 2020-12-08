package bab_conversie;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;


public class MainClass {

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		
		//String mapName = "N:\\databaseproject\\LexiconTool Cobalt Mathieu\\BrievenAlsBuit_textfiles_voor_herstel\\"; 
		String mapName = args[0]; // "N://Taalbank/CL-SE-data/Corpora/LexiconToolCorpora/Zeebrieven/AA bestanden 2014/EersteStukjeVliet41";
		String targetFolder = args[1];
			//"N:\\databaseproject\\LexiconToolMathieu\\BrievenAlsBuit\\";			  
			//"C:\\Users\\Mathieu\\Computerzaken\\INL/BrievenAlsBuitAlles\\";
		//"X:\\Projecten\\Impact\\LexiconTool\\BrievenAlsBuit\\tmp\\";
			//

		System.err.println("source folder: <" + mapName + ">");

		File file = new File(mapName);
		File[] files = file.listFiles();
		
//		DotWordsLogger dwl = new DotWordsLogger(6);
		
		for (int fileNr = 0; fileNr < files.length; fileNr++) 
		{
			String filename = files[fileNr].toString();
			String filenameWithoutPath = files[fileNr].getName().toString();
			System.err.println("Found txt file:" + filenameWithoutPath);
			//System.out.println(filename);

			if ( (!files[fileNr].isFile()) || filenameWithoutPath.startsWith(".") || //!filenameWithoutPath.equals("2-1-2008 023-026-TR-def.txt") ||
					!filename.endsWith(".txt"))
				continue;
			
			OriginalFileReader ofr = new OriginalFileReader();
			String[] headerInfoAndText = ofr.readFile(filename);
			
			Tokenizer tok = new Tokenizer(false);
			ArrayList<String[]> tokens = tok.tokenize(headerInfoAndText[1]);
			
			TeiBuilder tb = new TeiBuilder();
			String teiText = tb.buildDoc(tokens, filename, filenameWithoutPath, headerInfoAndText[0]);
			//System.err.println(teiText);
			tb.writeToUtf8File(teiText, targetFolder, filenameWithoutPath);
			
//			dwl.buildLog(filenameWithoutPath, tokens);
			
			
//			int teller = 0;
//			for (String[] word : tokens)
//			{
//				teller++;
//				if (teller>5)
//				{
//					teller = 0;
//					System.out.println();
//				}
//				System.out.print(" # "+word[0]);
//			}
		}
		
//		dwl.writeLog(mapName+"allFiles.abbreviations.log");

		
	}

}
