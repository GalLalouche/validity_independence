package parsers;

import java.io.*;
import java.util.Iterator;

import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.ss.usermodel.*;

@SuppressWarnings("all")
public class XlstoCSV {
	public static void toCsv(File inputFile, File outputFile) throws IOException {
		// For storing data into CSV files
		StringBuilder data = new StringBuilder();
		outputFile.createNewFile();
		FileOutputStream fos = new FileOutputStream(outputFile);

		// Get the workbook object for XLS file
		HSSFWorkbook workbook = new HSSFWorkbook(new FileInputStream(inputFile));
		// Get first sheet from the workbook
		HSSFSheet sheet = workbook.getSheetAt(0);
		Cell cell;
		Row row;

		// Iterate through each rows from first sheet
		Iterator<Row> rowIterator = sheet.iterator();
		while (rowIterator.hasNext()) {
			row = rowIterator.next();
			// For each row, iterate through each columns
			Iterator<Cell> cellIterator = row.cellIterator();
			while (cellIterator.hasNext()) {
				cell = cellIterator.next();

				switch (cell.getCellType()) {
				case Cell.CELL_TYPE_BOOLEAN:
					data.append(cell.getBooleanCellValue() + ",");
					break;

				case Cell.CELL_TYPE_NUMERIC:
					data.append(cell.getNumericCellValue() + ",");
					break;

				case Cell.CELL_TYPE_STRING:
					data.append(cell.getStringCellValue() + ",");
					break;

				case Cell.CELL_TYPE_BLANK:
					data.append("" + ",");
					break;

				default:
					data.append(cell + ",");
				}

			}
            data.append('\n');
        }

		fos.write(data.toString().getBytes());
		fos.close();
	}
}