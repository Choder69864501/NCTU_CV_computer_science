#include<iostream>
#include<ctime>
#define endl '\n'
using namespace std;
int main(){
	freopen("high_input.txt","w",stdout);
	int a,b,k,m;
	srand(time(NULL));
	for(int i=0;i<1000000;i++){
		a=rand()%100+1;
		b=rand()%100+1;
		m=rand()%1000000006+1;
		k=rand()%1000000+1;
		if(a>b)swap(a,b);	
		cout<<a<<" "<<b<<" "<<k<<" "<<m<<endl;
	}
	return 0;
}
