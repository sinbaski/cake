#include "DataSource.hpp"

BufferBufferedMySQL::BufferedMySQL(string username, string passwd, string database, string host)
{
    RInside R = RInside.instance();
    string stmt = "rm(list=ls()); source(\"./queryDatabase.r\"); ";
    stmt += "queryDatabase(" + username + "," + passwd + "," + database + "," + host;
    stmt += ", symbols, d1, d2)";
}

NumericMatrix BufferedMySQL::fetch(const vector<string> &symbols)
{
}
