#include <iostream>
#include <string>
#include <chrono>
#include <tuple>
#include <complex>
#include <Eigen/Dense>
#include <OpenXLSX.hpp>

using namespace std;
using namespace std::chrono;
using namespace OpenXLSX;

//using Eigen::MatrixXcf;
using namespace Eigen;


void insert_gap()
{
	std::cout << ' ' << endl;
}

void insert_start_separator()
{
	std::cout << ' ' << endl;
	std::cout << "======================== *START* ========================" << endl;
	std::cout << ' ' << endl;
}

void insert_end_separator()
{
	std::cout << ' ' << endl;
	std::cout << "========================= *END* =========================" << endl;
	std::cout << ' ' << endl;
}


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

// Класс для составления диапазона для определенного присоединения
class ProsoedRowRange 
{
public:
	tuple<int, int> get_range(int which)
	{
		tuple <int, int> pris_range;

		pris_range = make_tuple(10, 15);
	}
};

//                         CT        DB        CT        DB        CT        DB 
string worksheet_names[6] = {"Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"};

const int num_pris = 6;
const int pris_num = 1; // 1 - only first, 2 - only second, 3 - only third, ... 0 - all of them
int num_phases = 3; 
int num_harms = 49; 
int num_recs = 560; // number of recors (indexes) for one prisoed
int titles_indexes[num_pris];
int sheets_counter = 1;
int titles_counter = 0;


int main() {
	
	auto start = high_resolution_clock::now();

	
	XLDocument doc;
	doc.open("./Promzona.xlsx");
	auto workbook = doc.workbook();
	auto check_worksheet = doc.workbook().worksheet(worksheet_names[0]);
	
	int worksheets_count = doc.workbook().worksheetCount();
	int columns_count = check_worksheet.columnCount();
	int rows_count = check_worksheet.rowCount();
	
	

	insert_start_separator();

	std::cout << "WorkSheets count: " << worksheets_count << endl;
	
	insert_gap();
	
	std::cout << "Columns count: " << columns_count << endl;
	std::cout << "Rows count: " << rows_count << endl;
	
	insert_gap();


	// Определение начал данных измерении для каждого присоединения
	for (auto& row : check_worksheet.rows())
	{
		auto cell_range = row.cells(1);
		for (auto& cell_value : cell_range)
		{
			bool is_title = cell_value.value().typeAsString() == "string";
			if (is_title)
			{
				titles_indexes[titles_counter] = cell_value.cellReference().row();
				titles_counter++;
			}	
		}
	}
	titles_counter = 0;

	insert_gap();
	
	/*for (int title_index: titles_indexes) 
	{
		cout << title_index << endl;
	}*/

	// Testing
	for (auto& worksheet_name : workbook.worksheetNames())
	{
		// for every SHEET... ["Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"]
		auto worksheet = workbook.worksheet(worksheet_name);
		//                         !custom range! Prisoed 
		for (auto& row : worksheet.rows(2, 2))
		{
			if (sheets_counter % 2 == 0)                        // EVEN SHEETS
			{
				auto cell_range = row.cells();
				for (auto& cell_value : cell_range)
				{
					std::cout << "Worksheet: " << worksheet_name << " || Row: " << cell_value.cellReference().row() << " || Column: " << cell_value.cellReference().column() << " || Value: " << cell_value.value() << endl;
				}
			}
			else                                                // ODD SHEETS
			{
				/*auto cell_range = row.cells();
				for (auto& cell_value : cell_range)
				{
					std::cout << "Worksheet: " << worksheet_name << " || Row: " << cell_value.cellReference().row() << " || Column: " << cell_value.cellReference().column() << " || Value: " << cell_value.value() << endl;
				}*/
			}
		}
		sheets_counter++;
	}
	sheets_counter = 1;

	insert_end_separator();

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	std::cout << "Took to execute: "  << duration.count() << " seconds." << endl;
	return 0;
}