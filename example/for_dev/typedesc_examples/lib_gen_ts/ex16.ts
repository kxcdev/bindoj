type person_no_mangling =
  | { kind: "Anonymous" }
  | { kind: "Student"; name: string; student_id: number }
  | { kind: "Teacher"; department: string; faculty_id: number; name: string }
  | { kind: "With_id"; arg: number };
type Student = { admissionYear: number; name: string };
type Unit = 1;
type WithInt53p = { value: number };
export type NestedRecord = { person1: person_no_mangling; student: Student; unit: Unit } & WithInt53p &
  person_no_mangling;
