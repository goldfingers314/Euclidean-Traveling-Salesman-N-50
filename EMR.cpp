#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <cstdio>
#include <queue>
#include <stack>
#include <set>
#include <map>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

#define PB push_back
#define POP pop_back
#define MP make_pair
#define X first
#define Y second
#define PI M_PI

using ll=long long;

//I realized much later on that I might have switched the lat and lng coords in the entire program.
//It still works out though (they're all polar anyway).

//Used code from here to read from csv file (and modified slightly)
//https://www.delftstack.com/howto/cpp/read-csv-file-in-cpp/
using std::cout; using std::cerr;
using std::endl; using std::string;
using std::ifstream; using std::ostringstream;
using std::istringstream;

string readFileIntoString(const string& path) {
    auto ss = ostringstream{};
    ifstream input_file(path);
    if (!input_file.is_open()) {
        cerr << "Could not open the file - '"
             << path << "'" << endl;
        exit(EXIT_FAILURE);
    }
    ss << input_file.rdbuf();
    return ss.str();
}

//takes in two coordinates using lat/lng and outputs distance. Pretty accurate (google's honolulu to boston differs by only 20ish miles)
//I could precompute distances and it would shave off a constant factor (pretty significant: turn 30 hours to 2 hours).
//If the data is balanced, it will run 10^11 iterations each side, which should take 100*c seconds, where c is some constant depdendent on constant factor functions (like latlngmetric)
//Right now, c=1000, which is very slow. If you get rid of this, c would be 100. 
//Optimizing for another issue later on (line 426...) would shave off another factor of 4=> c=25.
//25*100 seconds to 25*1000 seconds => 41 min to 8 hours.
double latlngmetric(double lat1, double lng1, double lat2, double lng2){
	lat1 = lat1/(180/PI);
	lng1 = lng1/(180/PI);
	lat2 = lat2/(180/PI);
	lng2 = lng2/(180/PI);
	return 3958.8*acos(sin(lat2)*sin(lat1)+cos(lat2)*cos(lat1)*cos(lng1 - lng2));
}

//silhouette score has been proved best way to choose k (for the value needed for k-meaans)
//I'm just choosing a k that has on average group sizes < 3 and #of groups < 19 so I can actually compute it.
//Turns out that number is 23.

//k-means first time: (selection)
map<int, vector<pair<double, double>>> kmeansfirst(int k, vector<pair<double, double>> V){
	int n = V.size();

	double minx = 900000;
	double maxx = -900000;
	double miny = 900000;
	double maxy = -900000;
	for(int i = 0; i < n; i++){
		if(minx > V[i].X){minx = V[i].X;}
		if(maxx < V[i].X){maxx = V[i].X;}
		if(miny > V[i].Y){miny = V[i].Y;}
		if(maxy < V[i].Y){maxy = V[i].Y;}
	}

	vector<pair<double, double>> inits;

	srand (time(NULL));
	double d1, d2;
	for(int i = 0; i < k; i++){
		d1 = minx+(maxx-minx)*((double)(1+rand()%k)/(double)(k+1));
		d2 = miny+(maxy-miny)*((double)(1+rand()%k)/(double)(k+1));
		inits.PB(MP(d1, d2));
	}

	map<int, vector<pair<double, double>>> curM;
	curM[-1] = inits;

	for(int i = 0; i < n; i++){
		double distmin = 900000;
		int cluster = 0;
		for(int j = 0; j < k; j++){
			double D = latlngmetric(V[i].X, V[i].Y, curM[-1][j].X, curM[-1][j].Y);
			if(distmin > D){
				distmin = D;
				cluster = j;
			}
		}
		curM[cluster].PB(V[i]);
	}
	
	return curM;
}


//k-means first time: convergence iteration
map<int, vector<pair<double, double>>> kmeansiter(int k, vector<pair<double, double>> V, map<int, vector<pair<double, double>>> curM){
	for(int i = 0; i < k; i++){
		double xsum = 0; double ysum = 0;
		int n = curM[i].size();
		double setisize = (double)n;
		if(n != 0){
			for(int j = 0; j < n; j++){
				xsum+=curM[i][j].X;
				ysum+=curM[i][j].Y;
			}
			xsum/=setisize;
			ysum/=setisize;
			curM[-1][i] = MP(xsum,ysum);
		}
	}

	for(int i = 0; i < k; i++){
		curM[i].clear();
	}

	for(int i = 0; i < V.size(); i++){
		double distmin = 900000;
		int cluster = 0;
		for(int j = 0; j < k; j++){
			double D = latlngmetric(V[i].X, V[i].Y, curM[-1][j].X, curM[-1][j].Y);
			if(distmin > D){
				distmin = D;
				cluster = j;
			}
		}
		curM[cluster].PB(V[i]);
	}

	return curM;
}

//k-means finish: convergence finish
map<int, vector<pair<double, double>>> kmeans(int k, vector<pair<double, double>> V){

	map<int, vector<pair<double, double>>> testcluster = kmeansfirst(k, V);
	map<int, vector<pair<double, double>>> testcluster2 = testcluster;

	bool verity = true;
	while(verity){
		verity = false;
		testcluster = kmeansiter(k, V, testcluster);
		for(int i = 0; i < k; i++){
			if(testcluster[-1][i] != testcluster2[-1][i]){
				verity = true;
				break;
			}
		}
		testcluster2 = testcluster;
	}

	return testcluster;
}

int factorial(int n){
	int ans = 1;
	for(int i = 2; i < n+1; i++){
		ans *=i;
	}
	return ans;
}

int fact[11];//I was going to use this array more than I did, so I stored it in array to reduce computation

//below is the data structure i will use for the dp map for each cluster:
//the first string is the start capital, the second is end capital, the middle is the optimal path given the start and end
map<pair<string, string>, pair<double, vector<string>>> clusterdp(vector<pair<double, double>> V, map<pair<double,double>, string> namefinder){
	map<pair<string, string>, pair<double, vector<string>>> M;
	if(V.size() == 1){
		vector<string> sc;
		M[MP(namefinder[V[0]], namefinder[V[0]])] = MP(0, sc);
		return M;
	}
	else if(V.size() == 2){
		vector<string> sc;
		M[MP(namefinder[V[0]], namefinder[V[1]])] = MP(latlngmetric(V[0].X, V[0].Y, V[1].X, V[1].Y), sc);
		M[MP(namefinder[V[1]], namefinder[V[0]])] = MP(latlngmetric(V[0].X, V[0].Y, V[1].X, V[1].Y), sc);
		return M;
	}
	else{
		for(int i = 0; i < V.size()-1; i++){
			for(int j = i+1; j < V.size(); j++){

				vector<pair<double, double>> subset;
				for(int k= 0; k < V.size(); k++){
					if(k != i && k != j){
						subset.PB(V[k]);
					}
				}
				sort(subset.begin(), subset.end());

				int sz = V.size()-2;
				int N = factorial(sz);

				double curmin = 90000000;

				//now you iterate through all orders for i to j
				for(int k = 0; k < N; k++){
					
					double temp = 0;
					temp+= latlngmetric(V[i].X, V[i].Y, subset[0].X, subset[0].Y);
					temp+= latlngmetric(V[j].X, V[j].Y, subset[sz-1].X, subset[sz-1].Y);

					for(int l = 0; l < sz-1; l++){
						temp+= latlngmetric(subset[l].X, subset[l].Y, subset[l+1].X, subset[l+1].Y);
					}

					if(temp < curmin){
						vector<string> curpath;
						for(int l = 0; l < sz; l++){
							curpath.PB(namefinder[subset[l]]);
						}

						M[MP(namefinder[V[i]], namefinder[V[j]])] = MP(temp, curpath);
						curmin = temp;
						curpath.clear();
					}

					next_permutation(subset.begin(), subset.end());

				}

				//you can just ignore this block and do the above in reverse. (optimal a to b == optimal b to a)
				//i think it would be slower if it counts to check if it exists and read the path backwards, hence the repetition
				curmin = 90000000;
				sort(subset.begin(), subset.end());
				//now you iterate through all orders for j to i
				for(int k = 0; k < N; k++){
					double temp = 0;
					temp+= latlngmetric(V[j].X, V[j].Y, subset[0].X, subset[0].Y);
					temp+= latlngmetric(V[i].X, V[i].Y, subset[sz-1].X, subset[sz-1].Y);

					for(int l = 0; l < sz-1; l++){
						temp+= latlngmetric(subset[l].X, subset[l].Y, subset[l+1].X, subset[l+1].Y);
					}

					if(temp < curmin){
						vector<string> curpath;
						for(int l = 0; l < sz; l++){
							curpath.PB(namefinder[subset[l]]);
						}

						M[MP(namefinder[V[j]], namefinder[V[i]])] = MP(temp, curpath);
						curmin = temp;
						curpath.clear();
					}
					next_permutation(subset.begin(), subset.end());
				}
			subset.clear();


			}
		}
		return M;
	}

}


pair<double, double> IOWA;
pair<double, double> DC;

map<string, pair<double, vector<string>>> westdp; //i'm declaring outside. might make it faster?

//iterate through each cluster and all the possible start and end points of each cluster
//find a min path length conditioned on the order of the cluster
//check if that path that ends at th last coodinate is better than the existing value for that end.
void westtspupdater(vector<int> clusterorder, map<int, vector<pair<double,double>>> WEST, map<pair<string, string>, pair<double, vector<string>>> memory, map<pair<double,double>, string> namefinder, map<string, pair<double, double>> revnamefinder, int n, pair<double, double> state){
	//you go through all possible strings in this order and pick the best one. (up to the n-1th cluster)
	//for the nth cluster you want to iterate through all endings and store that in the westdp map

	queue<pair<double, vector<string>>> Q;

	//Connecting the start/end state to the first cluster
	if(WEST[clusterorder[0]].size()==1){
		vector<string> temppath;
		temppath.PB(namefinder[state]);
		temppath.PB(namefinder[WEST[clusterorder[0]][0]]);
		Q.push( MP(latlngmetric(IOWA.X, IOWA.Y, WEST[clusterorder[0]][0].X, WEST[clusterorder[0]][0].Y), temppath));
		temppath.clear();
	}
	else{
		for(int i = 0; i < WEST[clusterorder[0]].size()-1; i++){
			for(int j = i+1; j < WEST[clusterorder[0]].size(); j++){
				vector<string> temppath;
				temppath.PB(namefinder[state]);
				temppath.PB(namefinder[WEST[clusterorder[0]][i]]);
				double temp = latlngmetric(IOWA.X, IOWA.Y, WEST[clusterorder[0]][i].X, WEST[clusterorder[0]][i].Y);
				temppath.insert(temppath.end(), memory[MP(namefinder[WEST[clusterorder[0]][i]], namefinder[WEST[clusterorder[0]][j]])].Y.begin(), memory[MP(namefinder[WEST[clusterorder[0]][i]], namefinder[WEST[clusterorder[0]][j]])].Y.end());
				temp+= memory[MP(namefinder[WEST[clusterorder[0]][i]], namefinder[WEST[clusterorder[0]][j]])].X;
				temppath.PB(namefinder[WEST[clusterorder[0]][j]]);
				Q.push( MP(temp, temppath));

				temppath.clear();
				temppath.PB(namefinder[state]);
				temppath.PB(namefinder[WEST[clusterorder[0]][j]]);
				temp = latlngmetric(IOWA.X, IOWA.Y, WEST[clusterorder[0]][j].X, WEST[clusterorder[0]][j].Y);
				temppath.insert(temppath.end(), memory[MP(namefinder[WEST[clusterorder[0]][j]], namefinder[WEST[clusterorder[0]][i]])].Y.begin(), memory[MP(namefinder[WEST[clusterorder[0]][j]], namefinder[WEST[clusterorder[0]][i]])].Y.end());
				temp+= memory[MP(namefinder[WEST[clusterorder[0]][j]], namefinder[WEST[clusterorder[0]][i]])].X;
				temppath.PB(namefinder[WEST[clusterorder[0]][i]]);
				Q.push( MP(temp, temppath));			
				temppath.clear();
			}
		}
	}
	
	//Connecting the middle clusters with each other
	for(int i = 1; i < n-1; i++){
		if(WEST[clusterorder[i]].size()==1){
			int sz = Q.size();
			for(int j = 0; j < sz; j++){
				double temp = Q.front().X+latlngmetric(WEST[clusterorder[i]][0].X, WEST[clusterorder[i]][0].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y );
				vector<string> temppath;
				temppath.insert(temppath.end(), Q.front().Y.begin(), Q.front().Y.end());
				temppath.PB(namefinder[WEST[clusterorder[i]][0]]);
				Q.push( MP( temp, temppath ) );
				Q.pop();
			}
		}
		else{
			int sz = Q.size();
			for(int r = 0; r < sz; r++){
				for(int p = 0; p < WEST[clusterorder[i]].size()-1; p++){
					for(int q = p+1; q < WEST[clusterorder[i]].size(); q++){
						vector<string> temppath;
						temppath.insert(temppath.end(), Q.front().Y.begin(), Q.front().Y.end());
						temppath.PB(namefinder[WEST[clusterorder[i]][p]]);
						temppath.insert(temppath.end(), memory[MP(namefinder[WEST[clusterorder[i]][p]], namefinder[WEST[clusterorder[i]][q]])].Y.begin(), memory[MP(namefinder[WEST[clusterorder[i]][p]], namefinder[WEST[clusterorder[i]][q]])].Y.end());
						temppath.PB(namefinder[WEST[clusterorder[i]][q]]);
						Q.push(MP( Q.front().X+latlngmetric(WEST[clusterorder[i]][p].X, WEST[clusterorder[i]][p].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y)+ memory[MP(namefinder[WEST[clusterorder[i]][p]],namefinder[WEST[clusterorder[i]][q]])].X, temppath));

						temppath.clear();
						temppath.insert(temppath.end(), Q.front().Y.begin(), Q.front().Y.end());
						temppath.PB(namefinder[WEST[clusterorder[i]][q]]);
						temppath.insert(temppath.end(), memory[MP(namefinder[WEST[clusterorder[i]][q]], namefinder[WEST[clusterorder[i]][p]])].Y.begin(), memory[MP(namefinder[WEST[clusterorder[i]][q]], namefinder[WEST[clusterorder[i]][p]])].Y.end());
						temppath.PB(namefinder[WEST[clusterorder[i]][p]]);
						Q.push(MP(Q.front().X+latlngmetric(WEST[clusterorder[i]][q].X, WEST[clusterorder[i]][q].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y)+ memory[MP(namefinder[WEST[clusterorder[i]][q]],namefinder[WEST[clusterorder[i]][p]])].X, temppath));
					}
				}
				Q.pop();
			}
		}
	}

	//Updating the westdp map using the optimal path to the last state of the last cluster
	if(WEST[clusterorder[n-1]].size()==1){
		int sz = Q.size();
		for(int i = 0; i < sz; i++){
			if(Q.front().X+latlngmetric(WEST[clusterorder[n-1]][0].X, WEST[clusterorder[n-1]][0].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y) < westdp[namefinder[WEST[clusterorder[n-1]][0]]].X){
				vector<string> temppath;
				temppath.insert(temppath.end(), Q.front().Y.begin(), Q.front().Y.end());
				temppath.PB(namefinder[WEST[clusterorder[n-1]][0]]);
				westdp[namefinder[WEST[clusterorder[n-1]][0]]] = MP(Q.front().X+latlngmetric(WEST[clusterorder[n-1]][0].X, WEST[clusterorder[n-1]][0].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y), temppath);
			}
			Q.pop();
		}

	}
	else{
		int sz = Q.size();
		for(int r = 0; r < sz; r++){
			for(int p = 0; p < WEST[clusterorder[n-1]].size()-1; p++){
				for(int q = p+1; q < WEST[clusterorder[n-1]].size(); q++){

					if(Q.front().X+latlngmetric(WEST[clusterorder[n-1]][p].X, WEST[clusterorder[n-1]][p].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y)+ memory[ MP(namefinder[WEST[clusterorder[n-1]][p]],namefinder[WEST[clusterorder[n-1]][q]])].X < westdp[namefinder[WEST[clusterorder[n-1]][q]]].X){
						vector<string> temppath;
						temppath.insert(temppath.end(), Q.front().Y.begin(), Q.front().Y.end());
						temppath.PB(namefinder[WEST[clusterorder[n-1]][p]]);
						temppath.insert(temppath.end(), memory[MP(namefinder[WEST[clusterorder[n-1]][p]], namefinder[WEST[clusterorder[n-1]][q]])].Y.begin(), memory[MP(namefinder[WEST[clusterorder[n-1]][p]], namefinder[WEST[clusterorder[n-1]][q]])].Y.end());
						temppath.PB(namefinder[WEST[clusterorder[n-1]][q]]);
						westdp[namefinder[WEST[clusterorder[n-1]][q]]] = MP(Q.front().X+latlngmetric(WEST[clusterorder[n-1]][p].X, WEST[clusterorder[n-1]][p].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y)+ memory[ MP(namefinder[WEST[clusterorder[n-1]][p]],namefinder[WEST[clusterorder[n-1]][q]])].X , temppath);
						temppath.clear();
					}

					if(Q.front().X+latlngmetric(WEST[clusterorder[n-1]][q].X, WEST[clusterorder[n-1]][q].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y)+ memory[MP( namefinder[WEST[clusterorder[n-1]][q]],namefinder[WEST[clusterorder[n-1]][p]])].X < westdp[namefinder[WEST[clusterorder[n-1]][p]]].X){
						vector<string> temppath;
						temppath.insert(temppath.end(), Q.front().Y.begin(), Q.front().Y.end());
						temppath.PB(namefinder[WEST[clusterorder[n-1]][q]]);
						temppath.insert(temppath.end(), memory[MP(namefinder[WEST[clusterorder[n-1]][q]], namefinder[WEST[clusterorder[n-1]][p]])].Y.begin(), memory[MP(namefinder[WEST[clusterorder[n-1]][q]], namefinder[WEST[clusterorder[n-1]][p]])].Y.end());
						temppath.PB(namefinder[WEST[clusterorder[n-1]][p]]);
						westdp[namefinder[WEST[clusterorder[n-1]][p]]] = MP(Q.front().X+latlngmetric(WEST[clusterorder[n-1]][q].X, WEST[clusterorder[n-1]][q].Y, revnamefinder[Q.front().Y[Q.front().Y.size()-1]].X ,revnamefinder[Q.front().Y[Q.front().Y.size()-1]].Y)+ memory[ MP(namefinder[WEST[clusterorder[n-1]][q]],namefinder[WEST[clusterorder[n-1]][p]])].X , temppath);
						temppath.clear();
					}
				}
			}
			Q.pop();
		}
	}
}


//Now I TSP on the left halfs and the right halfs and store memory (the main function)
//I initially was going to code separate maps for each region, but then I realized I could just use one map for both regions.
//Hence my functions all use "west" terminology
void westtsp(map<int, vector<pair<double,double>>> WEST, map<pair<string, string>, pair<double, vector<string>>> memory, map<pair<double,double>, string> namefinder, map<string, pair<double, double>> revnamefinder, int n, pair<double, double> state){
	
	int N = fact[n];
	vector<int> clusterorder(n);
	for(int i = 0; i < n; i++){
		clusterorder[i] = i;
	}
	//it will start at the start/end state and finish at one of the capitals in the last cluster

	vector<string> curpath;

	for(int i = 0; i < n; i++){
		for(int j = 0; j < WEST[i].size(); j++){
			westdp[namefinder[WEST[i][j]]] = MP(90000000, curpath);
		}
	}

	for(int i = 0; i < N; i++){
		//you can can ignore all permutations in which clusters 0,1,2,3,4 are in first pos since you don't immediately need to travel to the western end of your region
		//likewise can ignore all permutations in which clusters 0,1,2,3,4 are in last pos. (cuts a factor of 4)

		if(clusterorder[0] != 0 && clusterorder[0] != 1 && clusterorder[0] != 2){
		 	if(clusterorder[n-1] != 0 && clusterorder[n-1] != 1 && clusterorder[n-1] != 2){
				westtspupdater(clusterorder, WEST, memory, namefinder, revnamefinder, n, state);
		 	}
		}

		next_permutation(clusterorder.begin(), clusterorder.end());

	}
}

int main(){

	//read the file and input the data:
    string filename("captial_coordinates.csv");
    string file_contents;
    map<int, vector<string>> csv_contents;
    char delimiter = ',';
    file_contents = readFileIntoString(filename);
    istringstream sstream(file_contents);
	vector<pair<string, pair<double, double>>> V;
	string d1, record; double d2, d3;

    while (std::getline(sstream, record)) {
        istringstream line(record);
        getline(line, record, delimiter);
        d1 = record;
        getline(line, record, delimiter);
        d2 = stod(record);
        getline(line, record, delimiter);
        d3 = stod(record);  
        V.PB(MP(d1, MP(d2, d3)));
    }


    map<pair<double,double>, string> namefinder;
    map<string, pair<double, double>> revnamefinder;

    //this is because Alaska and Hawaii are outliers, so I'm ignoring those for k-means since they deserve their own clusters anyways.
    //also it's pretty clear how to add those two optimally given a path. (i'm going to do this at the end)
    //(potential source of error from assumption: might not be optimal once you consider these, but it shouldn't deviate too far, and also I don't think this will happen.)

    //i also think it would be easier to keep iowa and DC as their own clusters and add them in later in the program.
   	//then I don't need edge cases in dealing with individual clusters (is iowa part of a cluster or not, how to deal with the two types of clusters, etc)
    vector<pair<double, double>> Vs;
    for(int i = 0; i < 47; i++){
    	Vs.PB(MP(V[i].Y.X, V[i].Y.Y));
    	namefinder[MP(V[i].Y.X, V[i].Y.Y)] = V[i].X;
    }

    for(int i = 47; i < 51; i++){
    	namefinder[MP(V[i].Y.X, V[i].Y.Y)] = V[i].X;
    }

    for(int i = 0; i < 51; i++){
    	revnamefinder[V[i].X] = MP(V[i].Y.X, V[i].Y.Y);
    }

    int k = 23;

    map<int, vector<pair<double, double>>> CLUSTERS = kmeans(k, Vs);

    map<int, vector<pair<double, double>>> fClusters;
    int cnt = 0;
    for(int i = 0; i < k; i++){
    	if(CLUSTERS[i].size() != 0){
    		fClusters[-1].PB(CLUSTERS[-1][i]);
 			for(int j= 0; j < CLUSTERS[i].size(); j++){
 				fClusters[cnt].PB(CLUSTERS[i][j]); 
 			}
 			cnt++;
    	}
    }
    int newk = fClusters[-1].size();

	vector< pair<pair<double,double>, int>> weastcluster;

	for(int i = 0; i < newk; i++){
		weastcluster.PB(MP(MP(fClusters[-1][i].Y, fClusters[-1][i].X), i));
	}

	sort(weastcluster.begin(), weastcluster.end());

	//this is where i'm storing the final iteration of all my clusters. (sorted and for everything excluding hawaii and alaska and iowa and DC)
    map<int, vector<pair<double, double>>> weastClusters;

    for(int i = 0; i < newk; i++){
    	weastClusters[-1].PB(MP(weastcluster[i].X.Y, weastcluster[i].X.X));
    	weastClusters[i] = fClusters[weastcluster[i].Y];
    }

    //initializes the fact array, which was supposed to reduce a constant factor until I opted for the next_permutation method of iterating through permutations
    for(int i = 1; i < 11; i++){
    	fact[i] = factorial(i);
    }

   	//just to reiterate, weastClusters is the map that contains the clusters (after sorting from west to east)
   	//now i need the map that contains the dp memory of all the clusters
   	map<pair<string, string>, pair<double, vector<string>>>memory;

   	for(int i = 0; i < newk; i++){
   		if(weastClusters[i].size() >= 10){
   			cout << "You need to run this program again: rarely the clusterizing process creates large clusters bigger than 9 states that are slow to deal with." << endl;
   			cout << "If you end up having to press this multiple times- your seed is bad and you just need to keep spamming. (like 1/3 to 1/4 of seeds are good)" << endl;
   			return 0;
   		}
   		map<pair<string, string>, pair<double, vector<string>>>tempmem = clusterdp(weastClusters[i], namefinder);
   		memory.insert(tempmem.begin(), tempmem.end());
   	}

   	//this is the code to print my memory map of maps. I commented it out and left it for posterity (useful for debugging when modifying algorithm)
   	// for(int i = 0; i < newk; i++){
   	// 	cout << i << endl;

   	// 	if(weastClusters[i].size()==1){
   	// 		cout << namefinder[weastClusters[i][0]] << ", " <<  namefinder[weastClusters[i][0]] << ", " << 0 << endl;
   	// 	}
   	// 	else{
	   // 		for(int j = 0; j < weastClusters[i].size()-1; j++){
	   // 			for(int k = j+1; k < weastClusters[i].size(); k++){

	   // 				cout << namefinder[weastClusters[i][j]] << ", ";

	   // 				for(int l = 0; l < memory[MP(namefinder[weastClusters[i][j]], namefinder[weastClusters[i][k]])].Y.size(); l++){
	   // 					cout << memory[MP(namefinder[weastClusters[i][j]], namefinder[weastClusters[i][k]])].Y[l] << ", ";
	   // 				}

	   // 				cout << namefinder[weastClusters[i][k]] << ", ";
	   // 				cout << memory[MP(namefinder[weastClusters[i][j]], namefinder[weastClusters[i][k]])].X << endl;



	   // 				cout << namefinder[weastClusters[i][k]] << ", ";

	   // 				for(int l = 0; l < memory[MP(namefinder[weastClusters[i][k]], namefinder[weastClusters[i][j]])].Y.size(); l++){
	   // 					cout << memory[MP(namefinder[weastClusters[i][k]], namefinder[weastClusters[i][j]])].Y[l] << ", ";
	   // 				}

	   // 				cout << namefinder[weastClusters[i][j]] << ", ";
	   // 				cout << memory[MP(namefinder[weastClusters[i][k]], namefinder[weastClusters[i][j]])].X << endl;
	   // 			}
   	// 		}
   	// 	}
   	// }

   	//inputting IOWA and DC
    IOWA = MP(V[50].Y.X, V[50].Y.Y);
    DC = MP(V[49].Y.X, V[49].Y.Y);

    //see comment on cluster sizes/etc on line 56-59
   	if(newk >18){
		cout << "You need to run this program again: The clusters are nicer and computationally feasible when there are less than 21 of them." << endl;
		cout << "If you end up having to press this multiple times- your seed is bad and you just need to keep spamming. (like 1/3 to 1/4 of seeds are good)" << endl;

		return 0;
   	}

   	//now I am going to separate the west part from the east by iowa's longitude
   	//there are usually 18+ clusters, so ideally 10 would go to west, and the rest to east. 
   	//(10 to west because the west because the west cluster sizes are usually smaller, and the east has massive ones in new england/etc)

   	map<int, vector<pair<double,double>>> WEST;
   	map<int, vector<pair<double,double>>> EAST;

   	//int wcnter = 0;

   	for(int i = 0; i < newk; i++){
   		if(i < 10){
   			WEST[i] = weastClusters[i];
   		}
   		else{
   			EAST[i-10] = weastClusters[i];
   		}
   	}


    cout << endl << "WEST" << endl;
   	for(int i = 0; i < 10; i++){
    	cout << endl << endl;
    	cout << i << endl;
    	for(int j = 0; j < WEST[i].size(); j++){
    	cout << namefinder[WEST[i][j]] << ", " << WEST[i][j].X << ", " << WEST[i][j].Y <<  endl;
    	}
    }
    cout << endl << "EAST" << endl;
    for(int i = 0; i < newk-10; i++){
    	cout << endl << endl;
    	cout << i << endl;
    	for(int j = 0; j < EAST[i].size(); j++){
    	cout << namefinder[EAST[i][j]] << ", " << EAST[i][j].X << ", " << EAST[i][j].Y <<  endl;
    	}
    }

    //int u = 10;
    int u = 5;

    //This creates the memory for the left halfs and right halfs
	westtsp(WEST, memory, namefinder, revnamefinder, u, IOWA);
	//westtsp(EAST, memory, namefinder, revnamefinder, newk-u, DC);
	westtsp(EAST, memory, namefinder, revnamefinder, u, DC);

	//here's westdp:

	for(int i = 0; i < u; i++){
		for(int j = 0; j < WEST[i].size(); j++){
			for(int k = 0; k < westdp[namefinder[WEST[i][j]]].Y.size(); k++){
				cout << westdp[namefinder[WEST[i][j]]].Y[k] << ", ";
			}cout << endl;
			cout << westdp[namefinder[WEST[i][j]]].X << endl << endl;
		}
	}

	for(int i = 0; i < u; i++){
		for(int j = 0; j < EAST[i].size(); j++){
			for(int k = 0; k < westdp[namefinder[EAST[i][j]]].Y.size(); k++){
				cout << westdp[namefinder[EAST[i][j]]].Y[k] << ", ";
			}cout << endl;
			cout << westdp[namefinder[EAST[i][j]]].X << endl << endl;
		}
	}

	//this uses both halves and connects them together to find the optimal path
	vector<string> finalpath;
	double finaldist = 90000000;
	for(int i = 0; i < u; i++){
		//for(int j = 0; j < newk-u; j++){
		for(int j = 0; j < u; j++){
			for(int k = 0; k < WEST[i].size(); k++){
				for(int l = 0; l < EAST[j].size(); l++){
					if(finaldist > westdp[namefinder[WEST[i][k]]].X + westdp[namefinder[EAST[j][l]]].X+latlngmetric(WEST[i][k].X, WEST[i][k].Y, EAST[j][l].X, EAST[j][l].Y)){
						finaldist = westdp[namefinder[WEST[i][k]]].X + westdp[namefinder[EAST[j][l]]].X+latlngmetric(WEST[i][k].X, WEST[i][k].Y, EAST[j][l].X, EAST[j][l].Y);
						finalpath.clear();
						finalpath.insert(finalpath.end(), westdp[namefinder[WEST[i][k]]].Y.begin(), westdp[namefinder[WEST[i][k]]].Y.end());
						for(int h = westdp[namefinder[EAST[j][l]]].Y.size()-1; h >=0; h--){
							finalpath.PB(westdp[namefinder[EAST[j][l]]].Y[h]);
						}
					}
				}
			}
		}
	}

	cout << "final path" << endl;
	for(int i = 0; i < finalpath.size(); i++){
		cout << finalpath[i] << ", ";
	}
	cout << endl << endl;
	cout << finaldist << endl;
	//This would give an optimal path under my cluster heurstic from iowa to DC excluding Hawaii and Alaska
	//You can easily extend the path to include Hawaii and Alaska (they're so far away from the other elements in the set you can just look at their immediate neighbors)
	return 0;
}