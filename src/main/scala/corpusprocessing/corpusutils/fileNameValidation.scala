package corpusprocessing.corpusutils
import java.util.Arrays

object fileNameValidation {

  object Platform {
    val OS_WIN32 = "win32"
    def getOS = "onbekend"
  }
  object OS {
    val playItSafe = true
    private var INSTALLED_PLATFORM: String = null
    var INVALID_RESOURCE_CHARACTERS: Array[Char] = null
    private var INVALID_RESOURCE_BASENAMES: Array[String] = null
    private var INVALID_RESOURCE_FULLNAMES: Array[String] = null

    /**
      * Returns true if the given name is a valid resource name on this operating system,
      * and false otherwise.
      */
    def isNameValid(name: String): Boolean = {
      //. and .. have special meaning on all platforms
      if (name == "." || name == "..") return false //$NON-NLS-1$ //$NON-NLS-2$

      if (INSTALLED_PLATFORM == Platform.OS_WIN32 || playItSafe) {
        //empty names are not valid
        val length = name.length
        if (length == 0) return false
        val lastChar = name.charAt(length - 1)
        // filenames ending in dot are not valid
        if (lastChar == '.') return false
        // file names ending with whitespace are truncated (bug 118997)
        if (Character.isWhitespace(lastChar)) return false
        val dot = name.indexOf('.')
        //on windows, filename suffixes are not relevant to name validity
        val basename = if (dot == -1) name
        else name.substring(0, dot)
        if (INVALID_RESOURCE_BASENAMES.contains(basename.toLowerCase)) return false
        return !(INVALID_RESOURCE_FULLNAMES.contains(basename.toLowerCase))
      }
      true
    }

    try
      //find out the OS being used
      //setup the invalid names
      INSTALLED_PLATFORM = Platform.getOS
    if (INSTALLED_PLATFORM == Platform.OS_WIN32 || playItSafe) {
      //valid names and characters taken from http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp
      INVALID_RESOURCE_CHARACTERS = Array[Char]('\\', '/', ':', '*', '?', '"', '<', '>', '|', '\0')
      INVALID_RESOURCE_BASENAMES = Array[String]("aux", "com1", "com2", "com3", "com4", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        "com5", "com6", "com7", "com8", "com9", "con", "lpt1", "lpt2", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
        "lpt3", "lpt4", "lpt5", "lpt6", "lpt7", "lpt8", "lpt9", "nul", "prn") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$

      //Arrays.sort(INVALID_RESOURCE_BASENAMES)
      //CLOCK$ may be used if an extension is provided
      INVALID_RESOURCE_FULLNAMES = Array[String]("clock$") //$NON-NLS-1$

    }
    else {
      //only front slash and null char are invalid on UNIXes
      //taken from http://www.faqs.org/faqs/unix-faq/faq/part2/section-2.html
      INVALID_RESOURCE_CHARACTERS = Array[Char]('/', '\0')
      INVALID_RESOURCE_BASENAMES = null
      INVALID_RESOURCE_FULLNAMES = null
    }

  }
}
