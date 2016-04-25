package metrics;

import java.util.*;

import org.eclipse.jdt.core.dom.*;

class CommentVisitor extends ASTVisitor {

    private final CompilationUnit compilationUnit;

    private final String[] source;
    private final int packageStart;

    private final List<String> comments = new LinkedList<>();

    public CommentVisitor(CompilationUnit compilationUnit, String[] source, int packageStart) {
        super();
        this.compilationUnit = compilationUnit;
        this.source = source;
        this.packageStart = packageStart;
    }

    @Override
	public boolean visit(LineComment node) {
        if (node.getStartPosition() < packageStart)
            return true;
        int startLineNumber = compilationUnit.getLineNumber(node.getStartPosition()) - 1;
        int startIndex = compilationUnit.getColumnNumber(node.getStartPosition());
		String lineComment = source[startLineNumber]
                .substring(startIndex)
                .substring(2) // removes //
                .trim();

        comments.add(lineComment);

        return true;
    }

    @Override
	public boolean visit(BlockComment node) {
        if (node.getStartPosition() < packageStart)
            return true;
        int startLineNumber = compilationUnit.getLineNumber(node.getStartPosition()) - 1;
        int startIndex = compilationUnit.getColumnNumber(node.getStartPosition());
        int endLineNumber = compilationUnit.getLineNumber(node.getStartPosition() + node.getLength()) - 1;
        int endIndex = compilationUnit.getColumnNumber(node.getStartPosition() + node.getLength());

        StringBuilder blockComment = new StringBuilder();

        for (int lineCount = startLineNumber; lineCount <= endLineNumber; lineCount++) {

            String originalLine = source[lineCount];
            String blockCommentLine = originalLine
                    .substring(0, lineCount < endLineNumber ? originalLine.length() : endIndex)
                    .substring(lineCount == startLineNumber ? startIndex : 0)
                    .trim();
            blockComment.append(blockCommentLine);
            if (lineCount != endLineNumber)
                blockComment.append("\n");
        }

        if (blockComment.toString().matches("\\/?[\\*\\s]*\\/?")) // ignores empty comments
            return true;
        comments.add(blockComment.toString().substring(2, blockComment.length() - 2));
        return true;
    }

    @Override
	public boolean visit(Javadoc node) {
        if (node.getStartPosition() < packageStart)
            return true;
        int startLineNumber = compilationUnit.getLineNumber(node.getStartPosition()) - 1;
        int startIndex = compilationUnit.getColumnNumber(node.getStartPosition());
        int endLineNumber = compilationUnit.getLineNumber(node.getStartPosition() + node.getLength()) - 1;
        int endIndex = compilationUnit.getColumnNumber(node.getStartPosition() + node.getLength());

        StringBuilder blockComment = new StringBuilder();

        for (int lineCount = startLineNumber; lineCount <= endLineNumber; lineCount++) {
            String originalLine = source[lineCount];
            String blockCommentLine = originalLine
                    .substring(0, lineCount < endLineNumber ? originalLine.length() : endIndex)
                    .substring(lineCount == startLineNumber ? startIndex : 0)
                    .trim()
                    .replaceAll("^(\\/?)\\*+", "$1")
                    .replaceAll("\\*+(\\/)?$", "$1");
            blockComment.append(blockCommentLine);
            if (lineCount != endLineNumber)
                blockComment.append("\n");
        }

        if (blockComment.toString().matches("\\/?[\\*\\s]*\\/?")) // ignores empty comments
            return true;
        comments.add(blockComment.toString().substring(1, blockComment.length() - 1));
        return true;
    }

    @Override
	public void preVisit(ASTNode node) {
    	// nothing
    }

    public Iterable<String> getComments() {
        return comments;
    }
}