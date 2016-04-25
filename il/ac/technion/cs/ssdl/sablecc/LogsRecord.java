package il.ac.technion.cs.ssdl.sablecc;

import java.io.File;
import java.util.Arrays;
import java.util.regex.Pattern;

public abstract class LogsRecord {
	public final File file;
	public final String revision;
	public final String previous;
	public final String comment;
	public final int[] diffs;

	public LogsRecord(final File file, final String revision, final String previous, final String comment, final int[] diffs) {
		this.file = file;
		this.revision = revision;
		this.previous = previous;
		this.comment = comment;
		this.diffs = diffs;
	}

	public static boolean isHeader(final String[] line, final Object[] objects) {
		int i = 0;
		for (final Object m : objects)
			if (!m.toString().equals(line[i++]))
				return false;
		return true;
	}

	private static Pattern bugsPattern = Pattern
	    .compile(
	        "((\\bFIXES\\b)|(\\bFIX\\b)|(\\bFIXING\\b)|(\\bFIXED\\b)|(\\bBUG\\b)|(\\bBUG\\b)|(\\b#?[1-9][0-9][0-9][0-9][0-9][0-9]?\\b))[.,:;]?",
	        Pattern.CASE_INSENSITIVE);

	public boolean includesLines(final int begin, final int end) {
		for (int i = begin; i <= end; i++)
			if (Arrays.binarySearch(diffs, i) > 0)
				return true;
		return false;
	}

	protected static boolean isBug(final String comment) {
		return bugsPattern.matcher(comment).find();
	}

	public boolean isBug() {
		final boolean $ = isBug(comment);
		return $;
	}

	@Override public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (comment == null ? 0 : comment.hashCode());
		result = prime * result + Arrays.hashCode(diffs);
		result = prime * result + (file == null ? 0 : file.hashCode());
		result = prime * result + (previous == null ? 0 : previous.hashCode());
		result = prime * result + (revision == null ? 0 : revision.hashCode());
		return result;
	}

	@Override public boolean equals(final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final LogsRecord other = (LogsRecord) obj;
		if (comment == null) {
			if (other.comment != null)
				return false;
		} else if (!comment.equals(other.comment))
			return false;
		if (!Arrays.equals(diffs, other.diffs))
			return false;
		if (file == null) {
			if (other.file != null)
				return false;
		} else if (!file.equals(other.file))
			return false;
		if (previous == null) {
			if (other.previous != null)
				return false;
		} else if (!previous.equals(other.previous))
			return false;
		if (revision == null) {
			if (other.revision != null)
				return false;
		} else if (!revision.equals(other.revision))
			return false;
		return true;
	}
}
