/**
 * Copyright 2011 The Open Source Research Group,
 *                University of Erlangen-Nürnberg
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.sweble.wikitext.engine;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;
import org.sweble.wikitext.engine.utils.CompilerTestBase;
import org.sweble.wikitext.lazy.LinkTargetException;

import de.fau.cs.osr.ptk.common.AstPrinter;

public class CompilerTest
		extends
			CompilerTestBase
{
	public CompilerTest() throws Exception
	{
		super();
	}
	
	@Test
	public void test()
			throws CompilerException, LinkTargetException, IOException
	{
		// FIXME: that's not enough and you know it ...
		preprocess("Test", false);
		postprocess("Regression:link-1", false);
	}
	
	@Test
	public void regresion()
			throws LinkTargetException, IOException, CompilerException
	{
		CompiledPage cp = postprocess("Boston Red Sox", false);
		Assert.assertEquals(
				"Page([\n" +
						"  Paragraph([\n" +
						"    Text(\"Founded in \")\n" +
						"    InternalLink(\n" +
						"      Properties:\n" +
						"            RTD = RtData: [0] = \"[[1901 in baseball\", [1] = \"]]\"\n" +
						"        {N} postfix = \"\"\n" +
						"        {N} prefix = \"\"\n" +
						"        {N} target = \"1901 in baseball\"\n" +
						"\n" +
						"      LinkTitle(\n" +
						"        Properties:\n" +
						"              RTD = RtData: [0] = \"|\", [1]\n" +
						"\n" +
						"        [ Text(\"1901\") ]\n" +
						"      )\n" +
						"    )\n" +
						"    Text(\" as one\")\n" +
						"  ])\n" +
						"])\n",
				AstPrinter.print(cp.getPage()));
	}
}
