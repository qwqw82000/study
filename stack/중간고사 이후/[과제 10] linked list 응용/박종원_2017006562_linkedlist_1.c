#include <stdio.h>

#include <stdlib.h>

 

typedef char element;

 

typedef struct ListNode {

	element data;

	struct  ListNode* link;

}ListNode;

 

ListNode* create_node(element data) {

	ListNode* new_node;

	new_node = (ListNode*)malloc(sizeof(ListNode));

 

	new_node->data = data; // 데이터 입력

	new_node->link = NULL; // NULL 링크 입력

 

	return new_node;

}

 

// phead: 리스트의 헤드 포인터의 포인터

// p: 선행 노드

// new_node: 삽입될 노드

 

void insert_node(ListNode** phead, ListNode* p, ListNode* new_node) {

	if (*phead == NULL) { // 공백리스트인 경우

		new_node->link = NULL;

		*phead = new_node;

	}

	else if (p == NULL) { // p가 NULL 이면 첫번째 노드로 삽입

		new_node->link = *phead;

		*phead = new_node;

	}

	else {               // p 다음에 삽입

		new_node->link = p->link;

		p->link = new_node;

	}

}

 

// phead: 헤드 포인터에 대한 포인터

// p: 삭제될 노드의 선행 노드

// removed: 삭제될 노드

 

void remove_node(ListNode** phead, ListNode* p, ListNode* removed) {

	if (p == NULL) // 공백리스트인 경우

		*phead = (*phead)->link;

	else           // p 다음 노드 제거

		p->link = removed->link;

	free(removed);

}

 

void display(ListNode* head) {

	ListNode* p = head;

 

	while (p != NULL) {

		printf("%c->", p->data);

		p = p->link; // p를 다음 노드로 이동

	}

	printf("\b\b  \n");

}

 

void display_recur(ListNode* head) {

	ListNode* p = head;

 

	if (p != NULL) {

		printf("%d->", p->data);

		display_recur(p->link); // p를 다음 노드로 이동

	}

}

 

ListNode* search(ListNode* head, int x) {

	ListNode* p;

	p = head;

 

	while (p != NULL) {

		if (p->data == x) return p; // 탐색 성공

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
	char s1[10];
	scanf("%s", s1);
	ListNode* head = NULL;

	int i=0;
	while (s1[i] != '\0'){
		insert_node(&head, NULL, create_node(s1[i++]));

	}
	display(head);
	ListNode* reverse_head = reverse(head);
	display(reverse_head);

 

	return 0;

}