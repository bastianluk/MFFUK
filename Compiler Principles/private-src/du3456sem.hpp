/*

	DU3456SEM.H

	DB

	Mlaskal's semantic interface for DU3-6

*/

#ifndef __DU3456SEM_H
#define __DU3456SEM_H

#include <string>
#include "literal_storage.hpp"
#include "flat_icblock.hpp"
#include "dutables.hpp"
#include "abstract_instr.hpp"
#include "gen_ainstr.hpp"
#include<tuple>
#include<cmath>
#include<cstdlib>

using namespace std;

namespace mlc {

	tuple<int, bool> parse_int(const string input);

	tuple<float, bool> parse_real(const string input);

	type_pointer get_type_pointer(symbol_tables* ctx, ls_id_index idx, int idx_line);

}

#endif
