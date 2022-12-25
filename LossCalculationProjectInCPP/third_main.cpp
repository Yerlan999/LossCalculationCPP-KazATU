#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <complex>
#include <Eigen/Dense>
#include <OpenXLSX.hpp>

using namespace std;
using namespace OpenXLSX;

using Eigen::MatrixXcf;

complex<double> mycomplex(10.0, 2.0);

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


MatrixXcf makeMatrix()
{
	MatrixXcf m(2, 2);
	m(0, 0) = mycomplex;
	m(1, 0) = 2.5;
	m(0, 1) = -1;
	m(1, 1) = m(1, 0) + m(0, 1);
	return m;
}

string sheetNames[6] = {"Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"};

int main() {

	MatrixXcf my_matrix = makeMatrix();

	cout << my_matrix << endl;

	XLDocument doc;
	doc.open("./Promzona.xlsx");
	auto wks = doc.workbook().worksheet(sheetNames[5]);
	double cell_value = wks.cell("A2").value();
	cout << cell_value << endl;

	return 0;
}