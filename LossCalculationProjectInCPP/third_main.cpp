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
	// Raw for now. In progress
public:
	tuple<int, int> get_range(int which)
	{
		tuple <int, int> pris_range;

		pris_range = make_tuple(10, 15);
		return pris_range;
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
int rows_counter = 0;

float UM[3][50][700];  float AIM[3][50][700];
float FUM[3][50][700]; float FIM[3][50][700];

float knsu[3][700]; float funu[3][700];
float knsi[3][700]; float funi[3][700];
float rmsu[3][700]; float fu[3][700];
float rmsi[3][700]; float fi[3][700];


class CustomRangePairs
{
public:
	int difference = -1;

	tuple<int, int> get_range_pairs(int i)
	{
		tuple <int, int> two_pairs;
		
		int first_part = i + difference;
		int second_part = first_part++;

		two_pairs = make_tuple(second_part, first_part);
		difference++;
		return two_pairs;
	}
	void reset() 
	{
		difference = -1;
	}
};


int main() {
	
	auto start = high_resolution_clock::now();

	XLDocument doc;
	doc.open("./Promzona.xlsx");
	auto workbook = doc.workbook();
	auto check_worksheet = doc.workbook().worksheet(worksheet_names[0]);
	
	int worksheets_count = doc.workbook().worksheetCount();
	int f_columns_count = check_worksheet.columnCount();
	int f_rows_count = check_worksheet.rowCount();

	insert_start_separator();
	std::cout << "Workbook's WorkSheets count: " << worksheets_count << endl;
	
	insert_gap();
	std::cout << "First Worksheet's Columns count: " << f_columns_count << endl;
	std::cout << "First Worksheet's Rows count: " << f_rows_count << endl;
	insert_gap();

	// Определение начал данных измерении для каждого присоединения
	for (auto& row : check_worksheet.rows())
	{
		for (auto& cell_value : row.cells(1))
		{
			if (cell_value.value().typeAsString() == "string")
			{
				titles_indexes[titles_counter] = cell_value.cellReference().row();
				titles_counter++;
			}	
		}
	}
	titles_counter = 0;


	// Стадия тестирования
	for (auto& worksheet_name : workbook.worksheetNames())
	{
		// for every SHEET... ["Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"]
		auto worksheet = workbook.worksheet(worksheet_name);
		int w_columns_count = worksheet.columnCount();
		int w_rows_count = worksheet.rowCount();
		
		insert_gap();
		std::cout << worksheet_name << "'s Columns count: " << w_columns_count << endl;
		std::cout << worksheet_name << "'s Rows count: " << w_rows_count << endl;
		insert_gap();


		//                      !custom range! Based on the Prisoed you want to calculate.
		for (auto& row : worksheet.rows(2, 2))
		{
			std::vector<XLCellValue> cell(row.values());

			if (sheets_counter % 2 == 0)                        // EVEN SHEETS
			{
				/*AIM[0][0][rows_counter] = cell.at(1);
				FIM[0][0][rows_counter] = cell.at(1);*/
			}
			else                                                // ODD SHEETS
			{
				CustomRangePairs ranger;
				for (int i = 1; i < (w_columns_count/2)+1; i++)
				{
					const auto [amp, pha] = ranger.get_range_pairs(i);
					cout << i << " || " << amp << " , " << pha << " ||" << endl;
					/*UM[0][i][rows_counter] = cell.at(amp);
					FUM[0][i][rows_counter] = cell.at(pha);*/
				
				}
				ranger.reset();

			}
			rows_counter++;
		}
		rows_counter = 0;
		sheets_counter++;
	}
	sheets_counter = 1;

	insert_end_separator();

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	std::cout << "Took to execute: "  << duration.count() << " seconds." << endl;
	return 0;
}