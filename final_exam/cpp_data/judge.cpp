#include<iostream>
#include<fstream>
#define endl '\n'
using namespace std;
int main(int argc,char*argv[]){
	fstream in1(argv[1]),in2(argv[2]);
	int a,b;
	cout<<argv[1]<<" "<<argv[2]<<endl;
	while(in1>>a&&in2>>b){
		cout<<a<<" "<<b<<endl;
		if(a!=b){
			cout<<"Answer Wrong!"<<endl;		
			return 0;
		}
	}
	cout<<"Accepted"<<endl;
	return 0;
}
