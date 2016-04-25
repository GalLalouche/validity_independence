package parsers;

import java.io.*;
import java.util.Iterator;

import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.*;

@SuppressWarnings("all")
class XlsxtoCSV {
	static void xlsx(File inputFile, File outputFile) {
		// For storing data into CSV files
		StringBuilder data = new StringBuilder();
		try {
			FileOutputStream fos = new FileOutputStream(outputFile);

			// Get the workbook object for XLSX file
			XSSFWorkbook wBook = new XSSFWorkbook(
					new FileInputStream(inputFile));

			// Get first sheet from the workbook
			XSSFSheet sheet = wBook.getSheetAt(0);
			Row row;
			Cell cell;

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
			}

			fos.write(data.toString().getBytes());
			fos.close();

		} catch (Exception ioe) {
			ioe.printStackTrace();
		}
	}
}
