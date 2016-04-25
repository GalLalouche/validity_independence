package il.ac.technion.cs.ssdl.sablecc;

import il.ac.technion.cs.ssdl.collections.IntsArray;
import il.ac.technion.cs.ssdl.utils.Separator;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

public class GitLogsRecord extends LogsRecord {
	public enum MAP {
		fileName, RevisionId, Date, Author, Comment, diffs;
		public String getString(final String[] line) {
			return line.length <= ordinal() ? null : line[ordinal()];
		}

		public int[] getInts(final String[] line) {
			try {
				return line.length <= ordinal() ? NO_INTEGERS : getInts(line[ordinal()]);
			} catch (final NumberFormatException e) {
				return null;
			}
		}

		private static int[] getInts(final String field) {
			if (0 == field.length())
				return NO_INTEGERS;
			final IntsArray $ = new IntsArray();
			for (final String s : field.split(";"))
				$.push(Integer.parseInt(s));
			return $.toArray();
		}

		public File getFile(final String[] line, final File folder) {
			return line.length <= ordinal() ? null : getFile(line[ordinal()], folder);
		}

		private static File getFile(final String field, final File folder) {
			return new File(folder, field.substring(field.startsWith("./") ? 2 : 0));
		}

		private static int[] NO_INTEGERS = new int[0];
	}

	public final String date;
	public final String author;

	public GitLogsRecord(final String[] line, final File folder) {
		this(MAP.fileName.getFile(line, folder), //
		    MAP.RevisionId.getString(line), //
		    MAP.Date.getString(line), //
		    MAP.Author.getString(line), //
		    MAP.Comment.getString(line), //
		    MAP.diffs.getInts(line));
	}

	@Override public String toString() {
		return "file = " + file + "\n" + //
		    "revision = " + revision + "\n" + //
		    "date = " + date + "\n" + //
		    "author = " + author + "\n" + //
		    "comment = " + comment + "\n" + //
		    "diffs = " + (diffs != null ? Separator.separateBy(diffs, ";") + "\n" : "null");//
	}

	public GitLogsRecord(final File file, final String revision, final String date, final String author, final String comment,
	    final int[] diffs) {
		super(file, revision, revision + "^", comment, diffs);
		this.date = date;
		this.author = author;
	}

	public static void main(final String args[]) throws IOException {
		final BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
		for (;;)
			System.out.println(isBug(r.readLine()));
	}

	@Override public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (author == null ? 0 : author.hashCode());
		result = prime * result + (date == null ? 0 : date.hashCode());
		return result;
	}

	@Override public boolean equals(final Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		final GitLogsRecord other = (GitLogsRecord) obj;
		if (author == null) {
			if (other.author != null)
				return false;
		} else if (!author.equals(other.author))
			return false;
		if (date == null) {
			if (other.date != null)
				return false;
		} else if (!date.equals(other.date))
			return false;
		return true;
	}
}
