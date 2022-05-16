import type { InferGetStaticPropsType, NextPage } from 'next'
import Head from 'next/head'
import styles from '../styles/Home.module.css'
import { getAllDocs } from "./api/fetch";

type Props = InferGetStaticPropsType<typeof getStaticProps>;

export const getStaticProps = async () => {
  const allDocs = getAllDocs();
  return {
    props: { allDocs },
  };
};

const Home: NextPage<Props> = ({ allDocs }) => {
  return (
    <div className={styles.container}>
      <Head>
        <title>bindoj</title>
        <meta name="description" content="Generated by create next app" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <h1 className={styles.title}>Docs</h1>
        <div className={styles.grid}>
          {allDocs.map((doc) => (
              <a href={doc.filename} className={styles.card} key={doc.filename}>
                <h2>{doc.filename}</h2>
              </a>
            ))}
        </div>
      </main>
    </div>
  )
}

export default Home