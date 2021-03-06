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

package org.sweble.wikitext.lazy.parser;

import de.fau.cs.osr.ptk.common.Warning;
import de.fau.cs.osr.ptk.common.ast.Span;

public abstract class LazyWarning
        extends
            Warning
{
	private static final long serialVersionUID = 1L;
	
	protected final WarningSeverity severity;
	
	// =========================================================================
	
	public LazyWarning(Span span, WarningSeverity severity, String origin, String message)
	{
		super(span, origin, message);
		this.severity = severity;
	}
	
	public LazyWarning(Span span, WarningSeverity severity, Class<?> origin, String message)
	{
		super(span, origin, message);
		this.severity = severity;
	}
	
	// =========================================================================
	
	public WarningSeverity getSeverity()
	{
		return severity;
	}
	
	// =========================================================================
	
	@Override
	public String toString()
	{
		String span = spanToString();
		String message = messageToString();
		String severity = severityToString();
		return "Warning (" + severity + "): " + span + " : " + message;
	}
	
	protected String severityToString()
	{
		String severity = "-";
		if (getSeverity() != null)
			severity = getSeverity().toString().toLowerCase();
		return severity;
	}
	
	// =========================================================================
	
	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((severity == null) ? 0 : severity.hashCode());
		return result;
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		LazyWarning other = (LazyWarning) obj;
		if (severity != other.severity)
			return false;
		return true;
	}
	
}
