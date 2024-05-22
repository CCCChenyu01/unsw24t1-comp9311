------------------------------------------------------
-- COMP9311 24T1 Project 1 
-- SQL and PL/pgSQL 
-- Template
-- Name:
-- zID:
------------------------------------------------------

-- Q1:
create or replace view Q1(subject_code)
as
select code 
from subjects
join orgunits on orgunits.id = subjects.offeredby
join orgunit_types on Orgunit_types.id =  orgunits.utype
where subjects.code ~ '^[A-Za-z]{4}7[0-9]{3}$'and orgunit_types.name like '%School%' and orgunits.longname like '%Information%'
;


-- Q2:
create or replace view Q2(course_id)
as
select courses.id
from courses
join classes on classes.course = courses.id
join class_types on class_types.id = classes.ctype
join subjects on subjects.id = courses.subject
where subjects.code like 'COMP%' 
and Class_types.name in('Lecture','Laboratory')
AND courses.id NOT IN (
    SELECT DISTINCT courses.id
    FROM courses
    JOIN classes ON classes.course = courses.id
    JOIN class_types ON class_types.id = classes.ctype
    WHERE class_types.name NOT IN ('Lecture', 'Laboratory')
)
group by courses.id
HAVING COUNT(DISTINCT Class_types.name) = 2
;


-- Q3:
create or replace view Q3(unsw_id)
as
SELECT p.unswid
FROM people p
JOIN students s ON s.id = p.id
JOIN course_enrolments ce ON ce.student = s.id
JOIN (
    SELECT c.id AS course_id
    FROM courses c
    JOIN course_staff cs ON cs.course = c.id
    JOIN staff sf ON sf.id = cs.staff
    JOIN people p ON p.id = sf.id
    WHERE p.title = 'Prof'
    GROUP BY c.id
    HAVING COUNT(DISTINCT p.id) >= 2
) cf ON cf.course_id = ce.course
JOIN courses c ON c.id = ce.course
JOIN semesters sm ON sm.id = c.semester
WHERE CAST(p.unswid AS TEXT) LIKE '320%' 
AND sm.year BETWEEN 2008 AND 2012
GROUP BY p.unswid
HAVING COUNT(DISTINCT cf.course_id) >= 5
;


-- Q4:
create or replace view Q4(course_id, avg_mark)
as
SELECT course_id, avg_mark
FROM (
    SELECT c.id AS course_id,
           ROUND(AVG(CASE WHEN ce.grade IN ('DN', 'HD') THEN ce.mark END)::numeric, 2) AS avg_mark,
           RANK() OVER (PARTITION BY s.term, o.name ORDER BY AVG(CASE WHEN ce.grade IN ('DN', 'HD') THEN ce.mark END) DESC) AS rank
    FROM courses c
    join course_enrolments ce on c.id = ce.course
    join semesters s on s.id = c.semester
    join subjects sj on sj.id = c.subject
    join orgunits o on o.id = sj.offeredby
    where s.year = 2012 
    and o.utype = 1
    and (ce.grade = 'DN' OR ce.grade = 'HD')
        GROUP BY s.term, o.name, c.id
) AS avg_marks_per_course
WHERE rank = 1
;


-- Q5:
create or replace view Q5(course_id, staff_name)
as
select c.id as course_id,
       STRING_AGG(distinct p.given, '; 'ORDER BY p.given) as staff_name
from courses c
join course_enrolments ce on c.id = ce.course
join semesters s on s.id = c.semester
join course_staff cs on cs.course = c.id
join staff sf on sf.id = cs.staff
join people p on p.id = sf.id
where s.year between 2005 and 2015
and p.title = 'Prof'
GROUP BY c.id
HAVING COUNT(DISTINCT ce.student) > 500
   AND COUNT(DISTINCT CASE WHEN p.title = 'Prof' THEN p.id END) >= 2
;


-- Q6:
create or replace view Q6(room_id, subject_code) 
as
WITH RoomClassCount AS (
    SELECT r.id AS room_id, COUNT(c.id) AS class_count
    FROM rooms r
    JOIN classes c ON c.room = r.id
    join courses cs on cs.id = c.course
    join semesters s on s.id = cs.semester
    where s.year = 2012
    GROUP BY r.id
),
MaxClassCount AS (
    SELECT MAX(class_count) AS max_class_count
    FROM RoomClassCount
),
RoomSubjectCount AS (
    SELECT rc.room_id, sj.code AS subject_code, COUNT(*) AS subject_count,
           RANK() OVER (PARTITION BY rc.room_id ORDER BY COUNT(*) DESC) AS rn
    FROM classes c
    JOIN courses cs ON cs.id = c.course
    JOIN subjects sj ON sj.id = cs.subject
    JOIN RoomClassCount rc ON c.room = rc.room_id
    JOIN MaxClassCount mc ON rc.class_count = mc.max_class_count
    JOIN semesters s ON s.id = cs.semester
    WHERE s.year = 2012
    GROUP BY rc.room_id, sj.code
)
SELECT room_id, subject_code
FROM RoomSubjectCount
WHERE rn = 1
;

-- Q7:

create or replace view Q7_1(id, program_id)
as
select pe.student as id, pe.program as program_id
from program_enrolments pe
group by pe.student, pe.program
having count(pe.student) >= 2
order by pe.student
;

create or replace view Q7_2(id, program, orgunit)
as 
select a2.id as id, a2.program_id as program, z.orgunit as orgunit
from
    (select a.id as id, o.id as orgunit
    from Q7_1 a 
    join programs p2 on p2.id = a.program_id
    join orgunits o on o.id = p2.offeredby
    group by a.id, o.id
    having count(o.id) >= 2
    )as z 
inner join Q7_1 a2 on a2.id = z.id
;

create or replace view Q7_3(id, program, orgunit, semester, starting, ending)
as 
select b.id, b.program, b.orgunit, pe2.semester as semester,
       sm.starting as starting, sm.ending as ending
from Q7_2 b
inner join program_enrolments pe2 on pe2.student = b.id 
inner join semesters sm on sm.id = pe2.semester
;

create or replace view Q7_4(id, program, semester)
as 
select y.id as id, d2.program as progam, d2.semester as semester
from
    (select d.id as id
    from Q7_3 d
    group by d.id
    having (MAX(ending) - MIN(starting)::DATE) < 1000
    ) as y 
inner join Q7_3 d2 on d2.id = y.id
order by y.id
;

create or replace view Q7_5(id, semester, sjuoc)
as 
select ce.student as id, c.semester as semester, 
            round(
       case when ce.mark >=50 then sj.uoc 
            else 0
        END)as sjuoc
from course_enrolments ce
inner join courses c on c.id = ce.course
inner join subjects sj on sj.id = c.subject
order by ce.student
;

create or replace view Q7_6(id, semester, sjuoc)
as 
select e.id as id, e.semester as semester, sum(e.sjuoc)
from Q7_5 e 
group by e.id, e.semester
order by e.id
;

create or replace view Q7_7(id, program, sjuoc)
as 
select f.id as id, f.program as progam,(sum(e.sjuoc)/2) as sjuoc
from Q7_4 f 
left join Q7_5 e on e.id = f.id and f.semester = e.semester
group by f.id, f.program
order by f.id
;

create or replace view Q7_8(id, program, sjuoc, uoc)
as 
select g.id as id, g.program as program, g.sjuoc as sjuoc, pg.uoc as uoc
from Q7_7 g 
inner join programs pg on pg.id = g.program
where sjuoc >= uoc
order by g.program 
;
create or replace view Q7_9(id)
as 
select i.id as id
from Q7_8 i 
group by i.id
having count(i.id) > 1
order by i.id 
;

create or replace view Q7_10(id)
as 
select j.id as id
from Q7_9 j 
inner join Q7_6 k on k.id = j.id
group by j.id
having count(j.id) < 6
;


create or replace view Q7(student_id, program_id)
as
select p.unswid as student_id, i.program as program_id
from Q7_10 h  
inner join people p on p.id = h.id
inner join Q7_8 i on i.id = h.id
;




-- Q8:
create or replace view Q8_1(orgunit, id)
as
SELECT a.orgunit as orgunit, a.staff as id 
FROM affiliations a
join staff_roles sr on sr.id = a.role
GROUP BY a.orgunit, a.staff
HAVING COUNT(*) >= 3
   and count(distinct sr.name) >= 3
;

create or replace view Q8_2(id, count)
as 
select a.staff, COUNT(*) AS count
from affiliations a
GROUP BY a.staff;

create or replace view Q8_3(id, hdn_rate)
as
select cs.staff, 
        ROUND(
    CASE 
        when SUM(CASE WHEN ce.mark IS NOT NULL THEN 1 ELSE 0 END) > 0
        THEN CAST(SUM(CASE WHEN mark >= 75 THEN 1 ELSE 0 END) * 1.0 / SUM(CASE WHEN ce.mark IS NOT NULL THEN 1 ELSE 0 END) AS numeric)
        ELSE NULL 
    END,
    2) AS hdn_rate
from course_staff cs 
inner join staff_roles sr on sr.id = cs.role
join courses c on c.id = cs.course
join course_enrolments ce on ce.course = c.id
join semesters sm on sm.id = c.semester
where sm.year = 2012 and sr.name = 'Course Convenor'
GROUP BY cs.staff;

create or replace view Q8_4(unswid, id)
as
select p.unswid, b.id
from Q8_1 b
inner join people p on p.id = b.id;

create or replace view Q8(staff_id, sum_roles, hdn_rate) 
as
WITH RankedQ8 AS (
    SELECT d.unswid AS staff_id, Q8_2.count AS sum_roles, Q8_3.hdn_rate AS hdn_rate,
           RANK() OVER (ORDER BY Q8_3.hdn_rate DESC) AS rank
    FROM Q8_4 d
    inner JOIN Q8_2 ON Q8_2.id = d.id
    inner JOIN Q8_3 ON Q8_3.id = d.id
    GROUP BY d.unswid, Q8_2.count, Q8_3.hdn_rate
)
SELECT staff_id, sum_roles, hdn_rate
FROM RankedQ8
WHERE rank <= 20;

-- Q9
CREATE OR REPLACE FUNCTION Q9(unswid integer) RETURNS SETOF text AS $$
DECLARE
    subject_code text;
    rank integer;
    course_rec record;
BEGIN
    -- Retrieve subject code and rank for the given student
    FOR course_rec IN
        SELECT
            f.code,
            f.rank
        FROM (
             SELECT ce.student AS id, b.course AS course, b.code as code, ce.mark as mark,
            CAST(RANK() OVER (PARTITION BY b.course ORDER BY ce.mark DESC) AS INTEGER) AS rank 
            FROM (SELECT course, s.code as code FROM course_enrolments ce
                JOIN courses c ON c.id = ce.course
                JOIN subjects s ON s.id = c.subject
                join people p on p.id = ce.student
                WHERE p.unswid = Q9.unswid AND mark IS NOT NULL
                AND s._prereq LIKE '%' || LEFT(CAST(s.code AS TEXT), 4) || '%'
                ) as b
            INNER JOIN course_enrolments ce ON ce.course = b.course
            WHERE ce.mark IS NOT NULL
        )as f
        JOIN
            people p ON p.id = f.id
        WHERE
            p.unswid = Q9.unswid
        ORDER BY
            rank
    LOOP
        -- Return subject code and rank
        RETURN NEXT course_rec.code || ' ' || course_rec.rank;
    END LOOP;

    -- Check if any courses match the criteria
    IF NOT FOUND THEN
        RETURN NEXT 'WARNING: Invalid Student Input [' || unswid || ']';
    END IF;
END;
$$ LANGUAGE plpgsql;


-- Q10
CREATE OR REPLACE FUNCTION Q10(unswid integer) RETURNS SETOF text AS $$
DECLARE
    program_rec RECORD;
    wam numeric := 0;
    total_units numeric := 0;
    divisor numeric := 0;
    total_wam numeric := 0;  -- Added variable declaration
BEGIN
    -- Check if the student has enrolled in any programs
    SELECT p.id
    INTO program_rec
    FROM program_enrolments pe
    JOIN people p ON pe.student = p.id
    WHERE p.unswid = Q10.unswid
    GROUP BY p.id;

    -- If no programs found, return warning
    IF NOT FOUND THEN
        RETURN NEXT 'WARNING: Invalid Student Input [' || unswid || ']';
        RETURN;
    END IF;

    -- Calculate WAM for each program
    FOR program_rec IN (
        select pg.name as program,g.wam as wam,g.uoc as uoc
        from(
            select b.program as program, 
                   SUM(CASE WHEN b.mark IS NOT NULL and b.grade not IN ('SY','XE','T','PE') THEN b.mark * b.uoc ELSE 0 END) as wam, 
                   SUM(CASE WHEN b.mark IS NOT NULL and b.grade not IN ('SY','XE','T','PE') THEN b.uoc ELSE 0 END) as uoc
            FROM (select ce.student as id, c.semester as semester,a.program_id as program ,ce.mark as mark , ce.grade as grade , s.uoc as uoc
				    from course_enrolments ce
                    join courses c on c.id = ce.course
                    join subjects s on s.id = c.subject
                    join (select pe.student as id, pe.program as program_id, pe.semester as semester
                        from program_enrolments pe
                        join people p2 on p2.id = pe.student
                        where p2.unswid = Q10.unswid) a on a.id = ce.student and a.semester = c.semester
			)as b
        group by b.program)as g 
        join programs pg on pg.id = g.program
    ) LOOP
        IF program_rec.uoc = 0 THEN
            RETURN NEXT unswid || ' ' || program_rec.program || ' ' || 'No WAM Available';
        ELSE
            RETURN NEXT unswid || ' ' || program_rec.program || ' ' || round((program_rec.wam) / (program_rec.uoc)::numeric, 2);
        END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql;