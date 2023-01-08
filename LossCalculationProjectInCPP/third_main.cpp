// Импортироване необходимых библиотек
#include <iostream>
#include <fstream>
#include <string>
#include <chrono>
#include <tuple>
#include <cmath>
#include <complex>
#include <Eigen/Dense>
#include <OpenXLSX.hpp>

using namespace std;
using namespace std::chrono;

using namespace OpenXLSX;
using namespace Eigen;

std::ofstream debug_file;

// Прочие функции для оформления вывода на консоль
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


// Функция для конвертации текста в цифру с плав. точкой
float stringToFloat(string s)
{
	float toFloat;
	stringstream converter(s);
	converter >> toFloat;
	return toFloat;
}

// Функция для конвертации текста в цифру
int stringToInt(string s)
{
	int toInt;
	stringstream converter(s);
	converter >> toInt;
	return toInt;
}


// Класс помощник для получения данных по Амплитуде и Фазе гармоник
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

// Класс помощник для получения данных по Фазам цепи
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


// Класс помощник для составления диапазона для определенного присоединения
class ProsoedRowRange 
{
public:
	tuple<int, int> get_range(int which, int titles_indexes[], int num_pris)
	{
		// which = [1-6] Если 6 присоединении в системе.
		tuple <int, int> pris_range;
		int r_start = titles_indexes[which - 1] + 1;
		int r_end = titles_indexes[which] - 1;
		
		if (which == num_pris) r_end++;
		pris_range = make_tuple(r_start, r_end);
		
		return pris_range;
	}
};

// Объявление основных переменных системы
const int num_pris = 6; // Общее количество присоединении в системе подстанции 
const int pris_num = 1; // 1 - для первой, 2 - для второй, 3 - третьей, .. (совершить расчет)
int num_phases = 3; 
int num_harms = 49; 
int num_recs = 560; // ~ Количество измерении в документе для каждого присоединения
int titles_indexes[(num_pris+1)];
int sheets_counter = 1;
int titles_counter = 0;
int rows_counter = 0;
int phase_number;

// Матрицы данных. Параметры режима
float UM[3][50][700] = {};  float AIM[3][50][700] = {};
float FUM[3][50][700] = {}; float FIM[3][50][700] = {};

float UM1[3][50][700] = {}; float UM2[3][50][700] = {};
float AIM1[3][50][700] = {}; float AIM2[3][50][700] = {};

float PPR1[700] = {}; float PPR2[700] = {};

float PD[3][700] = {}; float PPP[50][1000] = {};
float PPP1[50][1000] = {}; float PPP5[50][1000] = {};
float PPP2[50][1000] = {}; float PPP6[50][1000] = {};
float PPP3[50][1000] = {}; float PPP7[50][1000] = {};
float PPP4[50][1000] = {}; float PPP8[50][1000] = {};

float WD[2][50] = {}; float II[10] = {};

enum main_harm_enum { knsu_e = 0, knsi_e, rmsu_e, rmsi_e, funu_e, funi_e, fu_e, fi_e };
float main_harm[8][3][700] = {};


// Данные о зазмемлении линии
int ground_config[16] = { 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 };

std::complex<float> UK1[8] = {}; std::complex<float> AIK1[8] = {};

std::complex<float> UK10(0, 0); std::complex<float> AIK10(0, 0);
std::complex<float> UK11(0, 0); std::complex<float> AIK11(0, 0);
std::complex<float> UK12(0, 0); std::complex<float> AIK12(0, 0);
std::complex<float> AL(-0.5, 0.866025);

float PI = 3.14159265358979f;

float RZ = 35.3;
float FF, SS2, SS0, PP1, PP2, RPR, PRP, WD0;

// Общие переменной для расчетной функции! [MM, M, M1, MT, M10, M20, PR, K1, K2, K3, N1, N2, N3, MPR, MTR, MMT]
// Данные о конфигурации опор
int MM = 5, M, M1, MT = 5, M10, M20, PR, K1, K2, K3, N1, N2, N3, MPR = 3, MTR = 1, MMT;
float DT = 2.5;


// Расчетная функция программы!
void raschet()
{

}

// Главная функция запуска программы!
int main() {

	auto start = high_resolution_clock::now();

	debug_file.open("debug.txt", std::ios_base::app);

	XLDocument doc;
	doc.open("./Promzona.xlsx"); // Открываем Excel файл
	auto workbook = doc.workbook();
	auto check_worksheet = workbook.worksheet(workbook.worksheetNames()[0]);

	int worksheets_count = doc.workbook().worksheetCount();
	int f_columns_count = check_worksheet.columnCount();
	int f_rows_count = check_worksheet.rowCount();

	// Вывод основных данных о Excel файле
	insert_start_separator();
	debug_file << "Workbook's WorkSheets count: " << worksheets_count << endl;

	insert_gap();
	debug_file << "First Worksheet's Columns count: " << f_columns_count << endl;
	debug_file << "First Worksheet's Rows count: " << f_rows_count << endl;
	insert_gap();

	// Определение начал и концов данных измерении для каждого присоединения в файле
	titles_counter = 0;
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
	titles_indexes[num_pris] = f_rows_count;

	PhaseSheetsHandler phaser;
	CustomRangePairs ranger;
	ProsoedRowRange prisoeder;

	const auto [first, second] = prisoeder.get_range(pris_num, titles_indexes, num_pris);

	// Чтение EXCEL файла и запись полученных данных
	for (auto& worksheet_name : workbook.worksheetNames())
	{
		// Для каждго листа в документе ... ["Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet5", "Sheet6"]
		auto worksheet = workbook.worksheet(worksheet_name);
		int w_columns_count = worksheet.columnCount();
		int w_rows_count = worksheet.rowCount();

		if (!(sheets_counter % 2 == 0)) { phase_number = phaser.get_phase_number(sheets_counter); }
		else { phase_number = (sheets_counter - 2) / 2; };

		// В зависимости от рассчитываемого присоединения...
		for (auto& row : worksheet.rows(first, second)) // для каждой строки данного присоединения
		{

			std::deque<XLCellValue> cell(row.values()); // выбор всех ячеек в данной строке

			if (sheets_counter % 2 == 0)                        // Четные листы [Sheet2, Sheet4, Sheet6]
			{
				// Амплитуды и фазы для Тока
				for (int h = 1; h < ((w_columns_count - (w_columns_count - (num_harms * 2))) / 2) + 1; h++) // h = [1-49]
				{
					const auto [amp, pha] = ranger.get_range_pairs(h);
					try { AIM[phase_number][h][rows_counter] = cell.at(amp).get<float>(); }
					catch (XLValueTypeError) { AIM[phase_number][h][rows_counter] = (float)cell.at(amp).get<int>(); }
					try { FIM[phase_number][h][rows_counter] = cell.at(pha).get<float>(); }
					catch (XLValueTypeError) { FIM[phase_number][h][rows_counter] = (float)cell.at(pha).get<int>(); }
				}

				// Оставшиеся (последние) 8 столбцов. Данные Основной Гармоники
				for (int i = 0; i < w_columns_count - (num_harms * 2); i++)
				{
					try { main_harm[i][phase_number][rows_counter] = cell.at(num_harms * 2 + i).get<float>(); }
					catch (XLValueTypeError) { main_harm[i][phase_number][rows_counter] = (float)cell.at(num_harms * 2 + i).get<int>(); }
				}

				ranger.reset();
			}
			else                                                // Нечетные листы [Sheet1, Sheet3, Sheet5]
			{
				// Амплитуды и фазы для Напряжения 
				for (int h = 1; h < (w_columns_count / 2) + 1; h++) // h = [1-49]
				{
					const auto [amp, pha] = ranger.get_range_pairs(h);
					try { UM[phase_number][h][rows_counter] = cell.at(amp).get<float>(); }
					catch (XLValueTypeError) { UM[phase_number][h][rows_counter] = (float)cell.at(amp).get<int>(); }
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


	// Листы EXCEL файла прочитаны. Предварительные матрицы составлены.
	// Проведение расчетов!

	// Необходимые енумераторы для матрицы основной гармоники
	main_harm_enum knsu, knsi, rmsu, rmsi, funu, funi, fu, fi;
	knsu = knsu_e; rmsu = rmsu_e; funu = funu_e; fu = fu_e;
	knsi = knsi_e; rmsi = rmsi_e; funi = funi_e; fi = fi_e;


	// Цикл #13 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			UM[p][0][r] = main_harm[funu][0][r] * sqrt(2);
			FUM[p][0][r] = main_harm[fu][0][r] * (PI / 180);
			UM1[p][0][r] = UM[p][0][r] * cos(FUM[p][0][r]);
			UM2[p][0][r] = UM[p][0][r] * sin(FUM[p][0][r]);
		};
	};
	// Цикл #14 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			for (int h = 1; h < num_harms; h++)
			{
				UM[p][h][r] = UM[p][h][r] * UM[p][0][r] / 100;
				FUM[p][h][r] = FUM[p][h][r] * PI / 180;
				UM1[p][h][r] = UM[p][h][r] * cos(FUM[p][h][r]);
				UM2[p][h][r] = UM[p][h][r] * sin(FUM[p][h][r]);
			}
		}
	}
	// Цикл #16 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			AIM[p][0][r] = main_harm[funi][p][r]*sqrt(2);
			FIM[p][0][r] = main_harm[fi][p][r]*PI/180;
			AIM1[p][0][r] = AIM[p][0][r]*cos(FIM[p][0][r]);
			AIM2[p][0][r] = AIM[p][0][r]*sin(FIM[p][0][r]);
		}
	}
	// Цикл #117 в Фортране
	for (int r = 0; r < num_recs; r++)
	{
		for (int p = 0; p < num_phases; p++)
		{
			for (int h = 1; h < num_harms; h++)
			{
				AIM[p][h][r] = AIM[p][h][r]*AIM[p][0][r]/100;
				FIM[p][h][r] = FIM[p][h][r]*PI/180;
				AIM1[p][h][r] = AIM[p][h][r]*cos(FIM[p][h][r]);
				AIM2[p][h][r] = AIM[p][h][r]*sin(FIM[p][h][r]);
			}
		}
	}


	insert_end_separator();

	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<seconds>(stop - start);
	debug_file << "Took to execute: " << duration.count() << " seconds." << std::endl;
	std::cout << "Execution has just finished!" << std::endl;
	return 0;
}