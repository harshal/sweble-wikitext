package org.example;

import java.io.File;
import java.util.List;

import joptsimple.OptionException;
import de.fau.cs.osr.utils.getopt.Options;

public class App
{
	public static void main(String[] args) throws Exception
	{
		setUp(args);
	}
	
	private static void setUp(String[] args) throws Exception
	{
		Options opt = new Options();
		
		// Serilization format
		
		opt.createOption('f', "format")
				.withDescription("Serialization format, one of 'json', 'xml' or 'java'")
				.withArgName("FORMAT")
				.withRequiredArg()
				.create();
		
		// Timings
		
		opt.createOption("timings")
				.withDescription("Time parsing, serialization, compression and deserialization (can take a long time!)")
				.create();
		
		// Parsing
		
		opt.createOption("auto-correct")
				.withDescription("Auto correct erroneous wikitext")
				.create();
		
		opt.createOption("warnings")
				.withDescription("Generate parser warnings")
				.create();
		
		// Postprocessing
		
		opt.createOption("strip-attrs")
				.withDescription("Srtip all attributes from AST nodes before serialization")
				.create();
		
		opt.createOption("strip-location")
				.withDescription("Strip location information from AST nodes before serialization")
				.create();
		
		opt.createOption("strip-rtd")
				.withDescription("Strip only RTD attributes from AST nodes before serialization")
				.create();
		
		opt.createOption("simplify")
				.withDescription("Collapse adjacent text nodes into one text node")
				.create();
		
		// Go Command Line!
		
		List<String> free;
		try
		{
			opt.parse(args);
			
			free = opt.getFreeArguments();
			if (free == null || free.isEmpty())
			{
				printHelp(opt);
				return;
			}
			
			opt.expected("format");
			
			opt.optional("timings");
			opt.optional("auto-correct");
			opt.optional("warnings");
			opt.optional("strip-attrs");
			opt.optional("strip-location");
			opt.optional("strip-rtd");
			opt.optional("simplify");
		}
		catch (OptionException e)
		{
			printHelp(opt);
			System.err.println();
			System.err.println(e.getMessage());
			return;
		}
		
		String methodName = opt.value("format");
		
		SerializationMethod method;
		if (methodName.toLowerCase().equals("java"))
		{
			method = SerializationMethod.JAVA;
		}
		else if (methodName.toLowerCase().equals("xml"))
		{
			method = SerializationMethod.XML;
		}
		else if (methodName.toLowerCase().equals("json"))
		{
			method = SerializationMethod.JSON;
		}
		else
		{
			printHelp(opt);
			return;
		}
		
		String fileTitle = free.get(0);
		
		// Go Go Go!
		
		run(
				opt,
				new File(fileTitle + ".wikitext"),
				new File(fileTitle + "." + method.getExt()),
				method,
				fileTitle);
	}
	
	private static void printHelp(Options opt)
	{
		System.err.println("Usage: java -jar swc-example-serialization-VERSION.jar -f FORMAT [OPTIONS] TITLE");
		System.err.println();
		System.err.println("  The program will look for a file called `TITLE.wikitext',");
		System.err.println("  parse the file and write the serialized version to a file");
		System.err.println("  with the same name and the approprate extension.");
		System.err.println();
		opt.help(System.err);
	}
	
	private static void run(
			Options opt,
			File source,
			File target,
			SerializationMethod method,
			String fileTitle) throws Exception
	{
		Serializer serializer = new Serializer(source);
		
		// Parsing
		serializer.setParserAutoCorrectEnabled(opt.has("auto-correct"));
		serializer.setParserWarningsEnabled(opt.has("warnings"));
		serializer.setParserRtdEnabled(!opt.has("strip-rtd"));
		
		// Postprocessing
		serializer.setPpStripLocations(opt.has("strip-location"));
		serializer.setPpStripAllAttributes(opt.has("strip-attrs"));
		serializer.setPpStripRtdAttributes(opt.has("strip-rtd"));
		serializer.setPpSimplifyAst(opt.has("simplify"));
		
		// Timings
		serializer.setQuiet(!opt.has("timings"));
		
		if (opt.has("timings"))
			serializer.roundTrip(method);
		
		serializer.serializeTo(method, target);
	}
}
