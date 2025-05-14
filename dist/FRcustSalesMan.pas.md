<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar e editar informações relacionadas a vendedores associados a clientes. Ele fornece uma interface de grade (grid) que permite visualizar, editar e adicionar dados de vendedores. O objetivo principal é facilitar a manipulação de dados de vendedores de forma organizada e eficiente.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid:** Utilizados para exibir e manipular dados em formato de grade.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **Bibliotecas Personalizadas (kneUtils, kneDialogFactory, etc.):** Para funcionalidades específicas.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `businessUnit` (Texto, somente leitura).
      - `formatOrc` (Texto, somente leitura).
      - `salesmanName` (Texto, somente leitura).
      - `salesman` (Texto, com editor personalizado `cxEDTfind`).
    - **Ações da Grade e seus Efeitos:**
      - Alteração de valores em células específicas.
      - Busca de vendedores por meio de um botão associado ao campo `salesman`.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar novos vendedores.
  - Editar informações de vendedores existentes.
  - Buscar vendedores por código ou nome.

* **Componentes Principais:**
  - **Grade (cxGrid):** Exibe os dados dos vendedores.
  - **Botão de Busca (cxEDTfind):** Permite buscar vendedores.
  - **Painel de Ações:** Contém botões para adicionar, aplicar e cancelar ações.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `se botão clicado então execute ACTaddExecute`.
  - Evento `OnEditValueChanged` de uma célula: `se valor da célula alterado então execute cxDBVtableEditValueChanged`.
  - Evento `OnButtonClick` do campo `salesman`: `se botão clicado então execute m_SetFindSalesMan`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente com o método `Create`.
  2. Configuração das propriedades da grade, como campos ocultos, somente leitura e ordem.
  3. Interação do usuário com a interface:
     - Clique no botão "Adicionar" chama `ACTaddExecute`.
     - Alteração de valores na grade chama `cxDBVtableEditValueChanged`.
     - Clique no botão de busca chama `m_SetFindSalesMan`.

* **Dados Necessários:**
  - Código do vendedor (`salesman`).
  - Nome do vendedor (`salesmanName`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre que o botão "Adicionar" for clicado.
  - **Editar:** Disponível ao alterar valores na grade.
  - **Buscar:** Disponível ao clicar no botão de busca no campo `salesman`.

* **Filtros Disponíveis:**
  - Busca por código do vendedor.
  - Busca por nome do vendedor.

* **Mensagens de Erro:**
  - "Vendedor não encontrado" se a busca não retornar resultados.
  - "Campo obrigatório não preenchido" se campos obrigatórios estiverem vazios.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - Campo `salesman`: Deve ser preenchido com um código válido.
  - Campo `salesmanName`: Deve ser preenchido com um nome válido.

---

## 5. Funções Principais:

* **ACTaddExecute:** Adiciona um novo vendedor.
* **cxDBVtableEditValueChanged:** Manipula alterações nos valores da grade.
* **m_SetFindSalesMan:** Abre um diálogo para buscar vendedores.

---

## 6. Consumo de Serviços API:

* **Nome do Serviço:** SalesManServiceUtils.
* **Endpoint:** Não especificado no código.
* **Dados Enviados:** Não especificado no código.
* **Dados Recebidos:** Não especificado no código.
* **Propósito:** Buscar informações de vendedores.
* **Tratamento de Erros:** Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Personalizados:**
  - `kneUtils`, `kneDialogFactory`, `kneConfigObjects`: Para funcionalidades específicas.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `businessUnit` (Texto, somente leitura).
  - `formatOrc` (Texto, somente leitura).
  - `salesmanName` (Texto, somente leitura).
  - `salesman` (Texto, com editor personalizado `cxEDTfind`).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `salesman` → Coluna `salesman`.
  - `salesmanName` → Coluna `salesmanName`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  procedure TFRAMEcustSalesMan.ACTaddExecute(Sender: TObject);
  begin
    // Lógica para adicionar um novo vendedor
  end;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>businessUnit</th>
        <th>formatOrc</th>
        <th>salesmanName</th>
        <th>salesman</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Unidade 1</td>
        <td>Formato A</td>
        <td>João Silva</td>
        <td>12345</td>
      </tr>
      <tr>
        <td>Unidade 2</td>
        <td>Formato B</td>
        <td>Maria Oliveira</td>
        <td>67890</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades da grade no método `Create`.
* Uso de `DefineHiddenFields`, `DefineReadOnlyFields` e `DefineOrderFields` para personalizar a exibição da grade.

---

## 12. Conclusão:

O código fornece uma interface eficiente para gerenciar vendedores associados a clientes. Ele utiliza componentes visuais avançados e bibliotecas personalizadas para oferecer uma experiência de usuário rica. No entanto, faltam detalhes sobre a integração com serviços externos e validações mais robustas.

---

## 13. Resumo Curto:

O código implementa uma interface de grade para gerenciar vendedores, permitindo adicionar, editar e buscar dados. Ele utiliza componentes visuais avançados e bibliotecas personalizadas para facilitar a manipulação de dados.#### **FRcustSalesMan.pas**

```
unit FRcustSalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneTypes;

type
  TFRAMEcustSalesMan = class(TFRAMEBaseGridEditSOA)
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_SetFindSalesMan(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeSalesMan(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

  end;

var
  FRAMEcustSalesMan: TFRAMEcustSalesMan;

implementation

uses
  kneUtils, kneFindDialog, kneDialogFactory, kneConfigObjects, kneFGFindUtils,
  FRfindCriteriaSalesMan,
  SalesManServiceUtils;

{$R *.dfm}

constructor TFRAMEcustSalesMan.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=customerCode';
  DataPacketName := 'SalesmanByProd';
  PropertyName := 'salesByProd';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Hidden Fields ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Read-Only Fields ........................................................
    DefineReadOnlyFields('businessUnit;formatOrc;salesmanName');  // [14-10-2015, #22272]
    // Order Fields ............................................................
    DefineOrderFields('businessUnit;formatOrc;salesman;salesmanName'); // [14-10-2015, #22272]
		// Custom Editors ..........................................................
    AddCustomField('salesman','cxEDTfind');

  end; //with

  cxEDTfind.Properties.OnButtonClick := m_SetFindSalesMan;
end;


procedure TFRAMEcustSalesMan.m_SetFindSalesMan(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin

  try
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find do
    begin
      with Options.DataSelection do
      begin
        FieldNameForCode := 'salesMan';
        FieldNamesForDesc.Clear;
        FieldNamesForDesc.Add('name');

        TargetDataSet := cxDBVtable.DataController.DataSource.DataSet;
        TargetFieldNameForCode := 'salesman';
        TargetFieldNamesForDesc.Clear;
        TargetFieldNamesForDesc.Add('salesmanName');

        UseTargetDataSet:= True;
```

#### **FRcustSalesMan.dfm**

```
inherited FRAMEcustSalesMan: TFRAMEcustSalesMan
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Left = 2
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
```
<!-- tabs:end -->

