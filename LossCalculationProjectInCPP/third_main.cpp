#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <chrono>
#include <sstream>
#include <array>
#include <typeinfo>
#include <complex>
#include <Eigen/Dense>
#include <OpenXLSX.hpp>

using namespace std;
using namespace std::chrono;
using namespace OpenXLSX;

using Eigen::MatrixXcf;
using namespace Eigen;

complex<float> mycomplex(10.0, -2.0);

// Моя функция для конвертации текста в цифру с плав. точкой
float stringToFloat(string s)
{
	float toFloat;
	stringstream converter(s);
	converter >> toFloat;
	return toFloat;
}

// Моя функция для конвертации текста в цифру
int stringToInt(string s)
{
	int toInt;
	stringstream converter(s);
	converter >> toInt;
	return toInt;
}


//                         CT        DB        CT        DB        CT        DB 
string worksheet_names[6] = {"Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"};
int rows_counter = 1;
int values_counter = 5;



int main() {
	
	auto start = high_resolution_clock::now();

	
	complex<float> matrix[3][50][560] = {};
	matrix[0][0][0] = mycomplex;


	Matrix <complex<float>, 3, 3 > my_matrix;
	my_matrix.setZero();
	my_matrix(1, 2) = mycomplex;
	
	
	std::cout << matrix[0][0] << endl;




	XLDocument doc;
	doc.open("./Promzona.xlsx");
	auto workbook = doc.workbook();
	auto worksheet = doc.workbook().worksheet(worksheet_names[0]);
	
	int worksheets_count = doc.workbook().worksheetCount();
	int columns_count = worksheet.columnCount();
	int rows_count = worksheet.rowCount();


	std::cout << ' ' << endl;
	std::cout << "WorkSheets count: " << worksheets_count << endl;
	std::cout << ' ' << endl;

	for (auto& worksheetName : workbook.worksheetNames()) {
		std::cout << "WorkSheet name: " << worksheetName << endl;
	}

	std::cout << ' ' << endl;
	
	std::cout << "Columns count: " << columns_count << endl;
	std::cout << "Rows count: " << rows_count << endl;
	
	std::cout << ' ' << endl;

	for (auto& row : worksheet.rows(1)) {
		// for every ROW in SHEET...
		
		auto cell_range = row.cells(1, 1); // Read across the COLUMN 
		for (auto& cell_value: cell_range) 
		{
			std::cout << cell_value.value().typeAsString() << " || " << cell_value.value() << endl;
		}
		
	}

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	std::cout << "Took to execute: "  << duration.count() << " seconds." << endl;
	return 0;
}