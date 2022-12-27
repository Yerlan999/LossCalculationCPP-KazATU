#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <typeinfo>
#include <complex>
#include <Eigen/Dense>
#include <OpenXLSX.hpp>

using namespace std;
using namespace OpenXLSX;

using Eigen::MatrixXcf;

complex<double> mycomplex(10.0, 2.0);

// ��� ������� ��� ����������� ������ � ����� � ����. ������
float stringToFloat(string s)
{
	float toFloat;
	stringstream converter(s);
	converter >> toFloat;
	return toFloat;
}

// ��� ������� ��� ����������� ������ � �����
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
//                         CT        DB        CT        DB        CT        DB 
string sheetNames[6] = {"Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"};
int rows_counter = 5;
int values_counter = 5;

int main() {

	MatrixXcf my_matrix = makeMatrix();

	cout << my_matrix << endl;

	XLDocument doc;
	doc.open("./Promzona.xlsx");
	auto wks = doc.workbook().worksheet(sheetNames[0]);
	
	//auto rng = wks.range(XLCellReference("A2"), XLCellReference("CT3500"));
	//cout << "Cell count: " << std::distance(rng.begin(), rng.end()) << endl;

	for (auto& row : wks.rows()) {
		// for every ROW in SHEET...
		for (auto& value : std::deque<XLCellValue>(row.values())) {
			// for every VALUE in ROW...
			
			cout << value.typeAsString() << " | " << value << endl;
			
			if (!values_counter) 
			{
				values_counter = 5;
				break;
			}
			values_counter--;
		}
		if (!rows_counter)
		{ 
			break;
		}
		rows_counter--;
		cout << endl;
	}

	//double cell_value = wks.cell("a2").value();
	//cout << cell_value << endl;

	return 0;
}