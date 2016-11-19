#include <iostream>
#include <iomanip>
#include <vector>
#include <fstream>
#include <algorithm>
#include <omp.h>
#include <cmath>

using namespace std;

struct Point {
public:
    Point() : x(0), y(0) {}
    Point(double x, double y) : x(x), y(y) {}
    double x;
    double y;
    friend ostream& operator<<(ostream& stream, const Point& point) {
        stream << setprecision(16) << point.x << " " << point.y;
        return stream;
    }
};

class NormalFormLine {
    // p : a * x + b * y + c = 0
    // A[a1, a2]
    // v(p, A) = abs(a * a1 + b * a2 + c) / sqrt(a * a + b * b)

    // 1 / sqrt(a * a + b * b) - used a ton
    //  => invsqrt = 1 / sqrt(a * a + b * b)
    double a, b, c;
    double invsqrt; // invsqrt = 1 / sqrt(a * a + b * b) netreba
public:
    NormalFormLine(Point *A, Point *B) {
        // normal vector = vector + swap
        a = (B->y - A->y) * -1;
        b = B->x - A->x;
        c = -a * A->x - b * A->y;
        invsqrt = 1 / sqrt(a * a + b * b); // useless ?
    }
    int compare(Point * P) {
        double res = a * P->x + b * P->y + c;
        if (res > 0) {
            return 1;
        }
        return -1;
    }
    double getDistance(Point * P) {
        double t = a * P->x + b * P->y + c;
        if (t < 0) t *= -1; // abs()
        return t * invsqrt; // je treba prenasobit vse stejnou kladnou hodnotou ? -> neni
    }
};

Point ** getPoints(string file, int * size) {
    Point ** points;

    fstream fs(file, fstream::in);
    char line[256];
    int number;
    double x, y;

    if(!fs.is_open()) {
        cout << "Failed to open " << file << endl;
        return NULL;
    }

    fs.getline(line, 256);
    fs >> number;

    *size = number;
    points = new Point*[number];

    while(number-- > 0) {
        fs >> x;
        fs >> y;
        points[number] = new Point(x, y);
    }

    fs.close();
    return points;
}

bool cmp(const Point * left, const Point * right) {
    if (left->x == right->x) return left->y < right->y;
    return left->x < right->x;
}

bool checkPoints(string file, vector<Point *> * myResult, int size) {
    int number;
    double x, y;
    fstream fs(file, fstream::in);
    if(!fs.is_open()) {
        cout << "Failed to open " << file << endl;
        return false;
    }

    fs >> number; // reading dimension number
    fs >> number; // reading points number

    if(number != size) {
        fs.close();
        return false;
    }

    Point ** res = new Point*[number];
    while(number-- > 0) {
        fs >> x;
        fs >> y;
        res[number] = new Point(x, y);
    }

    sort(myResult->begin(), myResult->end(), cmp);
    sort(res, res + size, cmp);

    for(int i = 0; i < size; ++i) {
        if (myResult->at(i)->x != res[i]->x || myResult->at(i)->y != res[i]->y) {
            fs.close();
            for(int i = 0; i < size; ++i) delete res[i];
            delete res;
            return false;
        }
    }

    fs.close();
    for(int i = 0; i < size; ++i) delete res[i];
    delete[] res;
    return true;
}

void findHull(Point * left, Point * right, vector<Point *> * points, int size, NormalFormLine * nfl, vector<Point *> * result, int * res_size) {
    if (left->x == right->x && left->y == right->y) return;         // mozna osetrit jeste v quickHull()
    double f = 0, temp;

    Point * furthest = NULL;
    for(int i = 0; i < size; ++i) {
        temp = nfl->getDistance(points->at(i));
        if(temp > f) {
            f = temp;
            furthest = points->at(i);
        }
    }

    if (furthest == NULL) {
        result->push_back(right);
        *res_size += 1;
        return;
    }

    NormalFormLine nflL(left, furthest);
    NormalFormLine nflR(furthest, right);

    vector<Point *> * L = new vector<Point *>();
    vector<Point *> * R = new vector<Point *>();

    int k, s1 = 0, s2 = 0;
    for(int i = 0; i < size; ++i) {
        k = nflL.compare(points->at(i));
        if(k > 0) {
            L->push_back(points->at(i));
            continue;
        }
        k = nflR.compare(points->at(i));
        if(k > 0) {
            R->push_back(points->at(i));
        }
    }

    findHull(left, furthest, L, s1, &nflL, result, res_size);
    delete[] L;
    findHull(furthest, right, R, s2, &nflR, result, res_size);
    delete[] R;
}

vector<Point *> * quickHull(Point ** points, int size, int * res_size) {
    // osetreni male size ?

    Point * leftmost = points[0], * rightmost = points[0], * topmost = points[0], * bottommost = points[0];

    // vynechavam moznost se vsesmi body v jedne line
    for (int i = 0; i < size; ++i) {
        if (points[i]->x < leftmost->x) {
            leftmost = points[i];
        } else if (points[i]->x > rightmost->x) {
            rightmost = points[i];
        }

        if (points[i]->y < bottommost->y) {
            bottommost = points[i];
        } else if (points[i]->y > topmost->y) {
            topmost = points[i];
        }
    }

    /*
    Point ** result = new Point*[size];

    Point ** LT = new Point*[size];
    Point ** TR = new Point*[size];
    Point ** RB = new Point*[size];
    Point ** BL = new Point*[size];
    */

    vector<Point *> * result = new vector<Point *>();
    vector<Point *> * LT = new vector<Point *>();
    vector<Point *> * TR = new vector<Point *>();
    vector<Point *> * RB = new vector<Point *>();
    vector<Point *> * BL = new vector<Point *>();

    int s1 = 0, s2 = 0, s3 = 0, s4 = 0;

    NormalFormLine nflLT(leftmost, topmost);
    NormalFormLine nflTR(topmost, rightmost);
    NormalFormLine nflRB(rightmost, bottommost);
    NormalFormLine nflBL(bottommost, leftmost);

    cout << "Prepared to sort" << endl;

    int k = 0;
    for(int i = 0; i < size; ++i) {
        k = nflLT.compare(points[i]);
        if (k > 0) {
            LT->push_back(points[i]);
            continue;
        }
        k = nflTR.compare(points[i]);
        if (k > 0) {
            TR->push_back(points[i]);
            continue;
        }
        k = nflRB.compare(points[i]);
        if (k > 0) {
            RB->push_back(points[i]);
            continue;
        }
        k = nflBL.compare(points[i]);
        if (k > 0) {
            BL->push_back(points[i]);
        }
    }

    cout << "Sorted" << endl;

    cout << "res_size : " << res_size << " *res_size : " << * res_size << endl;

    *res_size = 0;
    findHull(leftmost, topmost, LT, s1, &nflLT, result, res_size);
    delete LT;
    cout << "LT" << endl;
    findHull(topmost, rightmost, TR, s2, &nflTR, result, res_size);
    delete TR;
    cout << "TR" << endl;
    findHull(rightmost, bottommost, RB, s3, &nflRB, result, res_size);
    delete RB;
    cout << "RB" << endl;
    findHull(bottommost, leftmost, BL, s4, &nflBL, result, res_size);
    delete BL;
    cout << "BL" << endl;

    return result;
}

int main (int argc, char* argv[]) {
    if(argc != 3) {
        cout << "usage : ./qh.o infile resfile" << endl;
        return 1;
    }
    string in_file  = argv[1];
    string res_file = argv[2];
    double start_time, end_time;

    cout << "              in_file : " << in_file  << endl;
    cout << "             res_file : " << res_file << endl;

    int size = 0, res_size = 0;
    Point ** points = getPoints(in_file, &size);

    if(points == NULL) return 2;

    start_time = omp_get_wtime();
    cout << "          Points read : " << size << endl;

    vector<Point *> * result = quickHull(points, size, &res_size);
    cout << "Points in convex hull : " << res_size << endl;

    end_time = omp_get_wtime();

    cout << "                 Time : " << end_time - start_time << "s" << endl;

    if (res_file != "0") {
        cout << "              results : ";
        if (checkPoints(res_file, result, res_size)) {
            cout << "correct" << endl;
        } else {
            cout << "incorrect" << endl;
        }
    }

    for(int i = 0; i < size; ++i) delete points[i];
    delete[] result;
    delete[] points;
    return 0;
}