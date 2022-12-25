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

using Eigen::MatrixXd;

complex<double> mycomplex(10.0, 2.0);


float stringToFloat(string s)
{
	float toFloat;
	stringstream converter(s);
	converter >> toFloat;
	return toFloat;
}


int stringToInt(string s)
{
	int toInt;
	stringstream converter(s);
	converter >> toInt;
	return toInt;
}


int makeMatrix()
{
	MatrixXd m(2, 2);
	m(0, 0) = 3;
	m(1, 0) = 2.5;
	m(0, 1) = -1;
	m(1, 1) = m(1, 0) + m(0, 1);
	cout << m << endl;
	return 0;
}



int main() {

	makeMatrix();

	cout << mycomplex << endl;

    XLDocument doc;
    doc.create("Spreadsheet.xlsx");
    auto wks = doc.workbook().worksheet("Sheet1");

    wks.cell("A1").value() = "Go and DO IT!";

    doc.save();

    return 0;
}