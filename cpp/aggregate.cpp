#include <vector>
#include <set>
#include <unordered_map>
#include <string>
#include <mysql.h>
#include <iostream>
#include <sstream>
#include <cstring>
#include <numeric>
#include <cassert>

using namespace std;

// class Asset
// {
// public:
//     string name;
//     vector<string> tables;
//     Asset(const string &s) : name(s){
//     };
//     Asset(const string &s, const string &tbl) : name(s) {
// 	tables.push_back(tbl);
//     };
// };

int main(int argc, char *argv[])
{
    unordered_map<string, vector<string>> assets;
    MYSQL *conn;
    conn = mysql_init(NULL);
    mysql_real_connect(conn, "127.0.0.1", "sinbaski", "q1w2e3r4",
		       "market", 0, NULL, 0);
    for (int i = 1; i < argc; i++) {
	string s ("select * from ");
	s += argv[i];
	s += " order by tm desc limit 1";
	int err = mysql_query(conn, s.c_str());
	MYSQL_RES *res = mysql_store_result(conn);
	auto n = mysql_num_fields(res);
	auto fields = mysql_fetch_fields(res);
	for (int j = 0; j < n; j++) {
	    string str = fields[j].name;
	    if (str == "tm" || str == "worth") continue;
	    if (str.rfind("_p") == str.size() - 2) continue;
	    str = str.substr(0, str.size() - 2);
	    if (assets.count(str) == 1) {
		assets[str].push_back(argv[i]);
	    } else {
		vector<string> v;
		v.push_back(string(argv[i]));
		assets.insert(make_pair(str, v));
	    }
	}
	mysql_free_result(res);
    }
    stringstream stmt;
    stmt << "select " << argv[1] << ".tm, ";
    for (auto it = assets.begin(); it != assets.end(); it = next(it)) {
    	stmt << it->second[0] << "." << it->first << "_p, ";
    	for (auto j = it->second.begin();
     	     j != it->second.end(); j = next(j)) {
     	    stmt << *j << "." << it->first << "_q/" << *j << ".worth";
     	    if (next(j) != it->second.end()) {
     		stmt << " + ";
    	    } else {
    		stmt << " as " << it->first << "_q ";
    	    }
     	}
    	if (next(it) != assets.end()) {
    	    stmt << ",";
    	}
    }
    stmt << " from ";
    for (int i = 1; i < argc; i++) {
      stmt << argv[i];
      if (i < argc - 1) {
	stmt << " join ";
      }
    }
    stmt << " on ";
    for (int i = 1; i < argc-1; i++) {
    	stmt << argv[i] << ".tm" << "=" << argv[i+1] << ".tm";
    	if (i < argc - 2) {
    	    stmt << " and ";
	}
    }
    printf("%s\n", stmt.str().c_str());
    vector<string> days;
    vector<double> cash{1.0};
    vector<double> V{1.0};
    unordered_map<string, double> holding1;
    unordered_map<string, double> holding2;
    unordered_map<string, double> prices;
    
    int err = mysql_query(conn, stmt.str().c_str());
    MYSQL_RES *res = mysql_store_result(conn);
    auto ncol = mysql_num_fields(res);
    auto fields = mysql_fetch_fields(res);
    for (auto j = assets.cbegin(); j != assets.cend(); j++) {
	holding1[j->first] = 0;
    }
    auto nrow = mysql_num_rows(res);
    V.resize(nrow);
    cash.resize(nrow);
    for (size_t i = 0; i < nrow; i++) {
	MYSQL_ROW row = mysql_fetch_row(res);
	// printf("\n");
	for (size_t j = 0; j < ncol; j++) {
	    // printf("%s\t", row[j]);
	    string name = fields[j].name;
	    if (name == "tm") {
		days.push_back(string(row[j]));
		continue;
	    }
	    string s = name.substr(0, name.size() - 2);
	    if (name.rfind("_p") == name.size() - 2) {
		prices[s] = stod(row[j]);
	    } else {
		holding2[s] = stod(row[j]);
	    }
	}
	// printf("\n");

	// for (auto j = assets.cbegin(); j != assets.cend(); j = next(j)) {
	//     printf("%s: %.2f, ", j->first.c_str(), prices[j->first]);
	// }
	// printf("\n");
	// for (auto j = assets.cbegin(); j != assets.cend(); j = next(j)) {
	//     printf("%s: %.2f, ", j->first.c_str(), holding2[j->first]);
	// }
	// printf("\n");

	V[i+1] = cash[i];
	for (auto j = assets.cbegin(); j != assets.cend(); j++) {
	    V[i+1] += prices[j->first] * holding1[j->first];
	}
	cash[i + 1] = V[i+1];
	for (auto j = assets.begin(); j != assets.end(); j++) {
	    double x = holding2[j->first] * V[i+1];
	    cash[i+1] -= x;
	    holding1[j->first] = x/prices[j->first];
	}
    }
    mysql_free_result(res);
    mysql_close(conn);
    for (auto i = V.cbegin(); i != V.cend(); i++) {
	cout << *i << endl;
    }
    cout << endl;
}
