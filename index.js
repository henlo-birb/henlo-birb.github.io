import './main.css';
import { Elm } from './src/Main.elm';
import jsPDF from 'jspdf'
import autoTable from 'jspdf-autotable'

const app = Elm.Main.init({
    node: document.getElementById('root')
});

function compareDays (a, b) {
    let val = x =>["M", "T", "W", "Th", "F"].findIndex(d => d==x)
    return val(a) > val(b)
}

app.ports.savePdf.subscribe(data => {
    let doc = new jsPDF()
    let classes = JSON.parse(data)
    classes.sort((a,b) => a["Start Time"] > b["Start Time"])
    classes.sort((a, b) => compareDays(a["Day"], b["Day"]))
    console.log(classes)
    doc.autoTable({
        columns: [
            {dataKey: "Class Name", header: "Class Name"},
            {dataKey: "Remote Status", header: "Remote Status"},
            {dataKey: "Level", header: "Level"},
            {dataKey: "Credits", header: "Credits"},
            {dataKey: "Day", header: "Day"},
            {dataKey: "Start Time", header: "Start Time"},
            {dataKey: "End Time", header: "End Time"}
        ],
        body: classes
    })
    doc.save("classes.pdf")
})
