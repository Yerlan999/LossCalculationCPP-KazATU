#include <iostream>
#include <fstream>
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

std::ofstream debug_file;

// ������ ������� ��� ���������� ������ �� �������
void insert_gap()
{
	debug_file << ' ' << endl;
}

void insert_start_separator()
{
	debug_file << " " << endl;
	debug_file << "======================== *START* ========================" << endl;
	debug_file << " " << endl;
}

void insert_end_separator()
{
	debug_file << " " << endl;
	debug_file << "========================= *END* =========================" << endl;
	debug_file << " " << endl;
}


// ������� ��� ����������� ������ � ����� � ����. ������
float stringToFloat(string s)
{
	float toFloat;
	stringstream converter(s);
	converter >> toFloat;
	return toFloat;
}

// ������� ��� ����������� ������ � �����
int stringToInt(string s)
{
	int toInt;
	stringstream converter(s);
	converter >> toInt;
	return toInt;
}


// ����� �������� ��� ��������� ������ �� ��������� � ���� ��������
class CustomRangePairs
{
public:
	int difference = -1;

	tuple<int, int> get_range_pairs(int harm)
	{
		tuple <int, int> two_pairs;

		int phase = harm + difference;
		int amplitude = phase++;

		two_pairs = make_tuple(amplitude, phase);
		difference++;
		return two_pairs;
	}

	void reset()
	{
		difference = -1;
	}
};

// ����� �������� ��� ��������� ������ �� ����� ����
class PhaseSheetsHandler
{
public:
	int odd_difference = -1;

	int get_phase_number(int sheet_number)
	{
		int phase_number = sheet_number + odd_difference;
		odd_difference--;
		return phase_number;
	}

	void reset()
	{
		odd_difference = -1;
	}
};


// ����� �������� ��� ����������� ��������� ��� ������������� �������������
class ProsoedRowRange 
{
public:
	tuple<int, int> get_range(int which, int titles_indexes[], int num_pris)
	{
		// which = [1-6] ���� 6 ������������� � �������.
		tuple <int, int> pris_range;
		int r_start = titles_indexes[which - 1] + 1;
		int r_end = titles_indexes[which] - 1;
		
		if (which == num_pris) r_end++;
		pris_range = make_tuple(r_start, r_end);
		
		return pris_range;
	}
};


const int num_pris = 6; // ����� ���������� ������������� � ������� ���������� 
const int pris_num = 1; // 1 - ��� ������, 2 - ��� ������, 3 - �������, .. (��������� ������)
int num_phases = 3; 
int num_harms = 49; 
int num_recs = 560; // ~ ���������� ��������� � ��������� ��� ������� �������������
int titles_indexes[(num_pris+1)];
int sheets_counter = 1;
int titles_counter = 0;
int rows_counter = 0;
int phase_number;


// ������� ������. ��������� ������
float UM[3][50][700] = {};  float AIM[3][50][700] = {};
float FUM[3][50][700] = {}; float FIM[3][50][700] = {};

float knsu[3][700] = {}; float funu[3][700] = {};
float knsi[3][700] = {}; float funi[3][700] = {};
float rmsu[3][700] = {}; float fu[3][700] = {};
float rmsi[3][700] = {}; float fi[3][700] = {};


int main() {
	
	auto start = high_resolution_clock::now();
	
	debug_file.open("debug.txt", std::ios_base::app);
	
	XLDocument doc;
	doc.open("./Promzona.xlsx");
	auto workbook = doc.workbook();
	auto check_worksheet = workbook.worksheet(workbook.worksheetNames()[0]);
	
	int worksheets_count = doc.workbook().worksheetCount();
	int f_columns_count = check_worksheet.columnCount();
	int f_rows_count = check_worksheet.rowCount();

	insert_start_separator();
	debug_file << "Workbook's WorkSheets count: " << worksheets_count << endl;
	
	insert_gap();
	debug_file << "First Worksheet's Columns count: " << f_columns_count << endl;
	debug_file << "First Worksheet's Rows count: " << f_rows_count << endl;
	insert_gap();

	// ����������� ����� ������ ��������� ��� ������� �������������
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
	titles_indexes[num_pris] = f_rows_count;

	PhaseSheetsHandler phaser;
	CustomRangePairs ranger;
	ProsoedRowRange prisoeder;
	
	const auto [first, second] = prisoeder.get_range(pris_num, titles_indexes, num_pris);
	
	// ������ ������������
	for (auto& worksheet_name : workbook.worksheetNames())
	{
		// ��� ������ ����� � ��������� ... ["Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"]
		auto worksheet = workbook.worksheet(worksheet_name);
		int w_columns_count = worksheet.columnCount();
		int w_rows_count = worksheet.rowCount();
		
		if (!(sheets_counter % 2 == 0)) { phase_number = phaser.get_phase_number(sheets_counter); }
		else { phase_number = (sheets_counter - 2) / 2; };

		// � ����������� �� ��������������� �������������...
		for (auto& row : worksheet.rows(first, second)) // ��� ������ ������ ������� �������������
		{

			std::deque<XLCellValue> cell(row.values()); // ����� ���� ����� � ������ ������
			
			if (sheets_counter % 2 == 0)                        // ������ ����� [Sheet2, Sheet4, Sheet6]
			{

				for (int h = 1; h < ((w_columns_count - (w_columns_count - (num_harms * 2)))/2)+1; h++) // h = [1-49]
				{
					const auto [amp, pha] = ranger.get_range_pairs(h);
					try { AIM[phase_number][h][rows_counter] = cell.at(amp).get<float>(); }
					catch (XLValueTypeError) { AIM[phase_number][h][rows_counter] = (float)cell.at(amp).get<int>(); }
					try { FIM[phase_number][h][rows_counter] = cell.at(pha).get<float>(); }
					catch (XLValueTypeError) { FIM[phase_number][h][rows_counter] = (float)cell.at(pha).get<int>(); }
				}
				
				// ���������� (���������) 8 ��������. ������ �������� ���������
				try { knsu[phase_number][rows_counter] = cell.at(w_columns_count - 8).get<float>(); }
				catch (XLValueTypeError) { knsu[phase_number][rows_counter] = (float)cell.at(w_columns_count - 8).get<int>(); }
				try { knsi[phase_number][rows_counter] = cell.at(w_columns_count - 7).get<float>(); }
				catch (XLValueTypeError) { knsi[phase_number][rows_counter] = (float)cell.at(w_columns_count - 7).get<int>(); }
				try { rmsu[phase_number][rows_counter] = cell.at(w_columns_count - 6).get<float>(); }
				catch (XLValueTypeError) { rmsu[phase_number][rows_counter] = (float)cell.at(w_columns_count - 6).get<int>(); }
				try { rmsi[phase_number][rows_counter] = cell.at(w_columns_count - 5).get<float>(); }
				catch (XLValueTypeError) { rmsi[phase_number][rows_counter] = (float)cell.at(w_columns_count - 5).get<int>(); }
				try { funu[phase_number][rows_counter] = cell.at(w_columns_count - 4).get<float>(); }
				catch (XLValueTypeError) { funu[phase_number][rows_counter] = (float)cell.at(w_columns_count - 4).get<int>(); }
				try { funi[phase_number][rows_counter] = cell.at(w_columns_count - 3).get<float>(); }
				catch (XLValueTypeError) { funi[phase_number][rows_counter] = (float)cell.at(w_columns_count - 3).get<int>(); }
				try { fu[phase_number][rows_counter] = cell.at(w_columns_count - 2).get<float>(); }
				catch (XLValueTypeError) { fu[phase_number][rows_counter] = (float)cell.at(w_columns_count - 2).get<int>(); }
				try { fi[phase_number][rows_counter] = cell.at(w_columns_count - 1).get<float>(); }
				catch (XLValueTypeError) { fi[phase_number][rows_counter] = (float)cell.at(w_columns_count - 1).get<int>(); }

				ranger.reset();
			}
			else                                                // �������� ����� [Sheet1, Sheet3, Sheet5]
			{
				
				for (int h = 1; h < (w_columns_count/2)+1; h++) // h = [1-49]
				{
					const auto [amp, pha] = ranger.get_range_pairs(h);
					try{UM[phase_number][h][rows_counter] = cell.at(amp).get<float>();}
					catch (XLValueTypeError){UM[phase_number][h][rows_counter] = (float)cell.at(amp).get<int>();}
					try { FUM[phase_number][h][rows_counter] = cell.at(pha).get<float>(); }
					catch (XLValueTypeError) { FUM[phase_number][h][rows_counter] = (float)cell.at(pha).get<int>(); }
				}
				ranger.reset();
			}
			rows_counter++;
		}
		rows_counter = 0;
		sheets_counter++;
	}
	phaser.reset();
	sheets_counter = 1;

	insert_end_separator();

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	debug_file << "Took to execute: "  << duration.count() << " seconds." << std::endl;
	std::cout << "Execution has just finished!" << std::endl;
	return 0;
}