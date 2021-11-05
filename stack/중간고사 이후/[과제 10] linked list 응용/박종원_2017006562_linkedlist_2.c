// 지하철 문제
#include <stdio.h>

#include <stdlib.h>

#include <string.h>

 

typedef int element;

 

typedef struct ListNode {

	char data[20];

	element dist;

	struct  ListNode* link;

}ListNode;

 

ListNode* create_node(char* station ,element data) {

	ListNode* new_node;

	new_node = (ListNode*)malloc(sizeof(ListNode));

	strcpy(new_node->data, station);// 역이름 복사


	new_node->dist = data; // 데이터 입력

	new_node->link = NULL; // NULL 링크 입력

 

	return new_node;

}

 

// phead: 리스트의 헤드 포인터의 포인터

// p: 선행 노드

// new_node: 삽입될 노드

 

void insert_node(ListNode** phead, ListNode* p, ListNode* new_node, int time) {

	if (*phead == NULL) { // 공백리스트인 경우

		new_node->link = NULL;

		*phead = new_node;

	}

	else if (p == NULL) { // p가 NULL 이면 첫번째 노드로 삽입

		new_node->link = *phead;

		p->dist = time;

		*phead = new_node;

	}

	else {               // p 다음에 삽입

		new_node->link = p->link;

		p->dist = time;

		p->link = new_node;

	}

}

 

// phead: 헤드 포인터에 대한 포인터

// p: 삭제될 노드의 선행 노드

// removed: 삭제될 노드

 

void remove_node(ListNode** phead, ListNode* p, ListNode* removed, int time) {

	if (p == NULL) // 공백리스트인 경우

		*phead = (*phead)->link;

	else           // p 다음 노드 제거

		p->link = removed->link;
		p->dist = time;

	free(removed);

}

 

void display(ListNode* head) {

	ListNode* p = head;

 

	while (p != NULL) {

		printf("%s(%d)->", p->data, p->dist);

		p = p->link; // p를 다음 노드로 이동

	}

	printf("\b\b  \n");

}


void Time(ListNode* head) {

	ListNode* p = head;
	int time = 0;
	p = p->link;

 

	while (p != NULL) {

		time += p->dist;

		p = p->link; // p를 다음 노드로 이동

	}

	printf("From Aewol to Seongsan: %d\n",time);

} 

void display_recur(ListNode* head) {

	ListNode* p = head;

 

	if (p != NULL) {

		printf("%d->", p->data);

		display_recur(p->link); // p를 다음 노드로 이동

	}

}

 

ListNode* search(ListNode* head, char *station) {

	ListNode* p;

	p = head;

 

	while (p != NULL) {

		if (strcmp(p->data,station) == 0) return p; // 탐색 성공

		p = p->link;

	}

 

	return p; // 탐색 실패일 경우 NULL 반환

}

 

ListNode* concat(ListNode* head1, ListNode* head2) {

	ListNode* p;

 

	if (head1 == NULL) return head2;

	else if (head2 == NULL) return head1;

	else {

		p = head1;

		while (p->link != NULL)

			p = p->link;

		p->link = head2;

 

		return head1;

	}

}

 

ListNode* reverse(ListNode* head) {

	// 순회 포인터로 p, q, r을 사용

	ListNode* p;

	ListNode* q;

	ListNode* r;

	p = head; // p는 아직 처리되지 않은 노드

	q = NULL; // q는 역순으로 만들 노드

 

	while (p != NULL) {

		r = q; // r 은 역순으로 된 노드.

		q = p; // r은 q, q는 p를 차례로 따라간다

		p = p->link; // p를 다음 노드로 이동

		q->link = r; // q의 링크 방향을 바꾼다

	}

 

	return q; // q는 역순으로 된 리스트의 헤드 포인터

}

 

int main()

{
	ListNode* head = NULL;
	insert_node(&head, NULL, create_node("Airport",0),0);

	display(head);

	insert_node(&head, search(head, "Airport"), create_node("Aewol",0),20);
	display(head);

	insert_node(&head, search(head, "Aewol"), create_node("Seogwipo",0),40);
	display(head);

	insert_node(&head, search(head, "Seogwipo"), create_node("Seongsan",0),30);
	display(head);

	insert_node(&head, search(head, "Aewol"), create_node("Moseulpo",30),30);
	display(head);

	Time(head);

	remove_node(&head, search(head, "Aewol"), search(head, "Moseulpo"),40);

	display(head);

	Time(head);





	
 

	return 0;

}