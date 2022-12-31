#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <complex>
#include <Eigen/Dense>


using namespace std;
using Eigen::MatrixXd;

complex<double> mycomplex(10.0, 2.0);
int x = 5;


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


int main()
{

	makeMatrix();
	
	cout << mycomplex << endl;

	string fname;  // Переменная для хранения названия CSV файла
	cout << "Enter the file name: ";  // Запрос ввести название файла
	cin >> fname;  // Запись названия в переменную

	vector<vector<string>> content; // Вектор в векторе 
	vector<string> row;
	string line, word;  // Создание переменных 

	fstream file(fname, ios::in); // Чтение CSV файла (в режиме чтения)
	if (file.is_open())
	{
		while (getline(file, line))
		{
			row.clear();

			stringstream str(line);

			while (getline(str, word, ','))
				row.push_back(word);
			content.push_back(row);
		}
	}
	else
		cout << "Could not open the file\n";

	for (int i = 0; i < content.size(); i++)
	{
		for (int j = 0; j < content[i].size(); j++)
		{
			cout << content[i][j] << " ";
		}
		cout << "\n";
	}

	return 0;
}