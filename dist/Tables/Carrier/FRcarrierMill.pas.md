<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código `FRcarrierMill` implementa uma interface gráfica para gerenciar dados relacionados a "Mills" e "Carriers" em um sistema. Ele fornece funcionalidades para exibir, editar, validar e salvar informações em um grid interativo. O objetivo principal é facilitar a manipulação de dados relacionados a "Mills" e "Carriers" de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação principal.
  - **Componentes VCL:** Incluindo `TcxGrid`, `TcxGridDBTableView`, e outros para a interface gráfica.
  - **SOAP:** Para comunicação com serviços externos.
  - **Banco de Dados:** Manipulação de dados via `TDataSet` e `TClientDataSet`.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e Tipos:**
      - `mill` (string): Identificador do Mill.
      - `millCarrier` (string): Relacionamento entre Mill e Carrier.
      - `stat` (string): Status.
      - `millDtime` (datetime): Data e hora.
      - `sapCode` (string): Código SAP.
    - **Ações da Grade e Efeitos:**
      - **Duplo clique em uma célula:** Abre detalhes ou edita o valor.
      - **Alteração de valor:** Valida e salva alterações.
      - **Mudança de registro selecionado:** Atualiza o estado da interface.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar novos registros.
  - Excluir registros existentes.
  - Editar valores diretamente na grade.
  - Validar e salvar alterações.
  - Pesquisar registros por código ou nome.

* **Componentes Principais:**
  - `TcxGridDBTableView`: Exibe os dados em formato de grade.
  - `TDataSet`: Gerencia os dados exibidos.
  - `TActionList`: Define ações como adicionar e excluir.

* **Tradução para Pseudo-código:**
  - Evento `OnCellDblClick`: `se célula for clicada duas vezes então abrir detalhes ou editar valor`.
  - Evento `OnEditValueChanged`: `se valor da célula for alterado então validar e salvar`.
  - Evento `OnFocusedRecordChanged`: `se registro selecionado mudar então atualizar estado da interface`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente (`Create`):
     - Configurações iniciais da grade.
     - Definição de campos chave e propriedades.
  2. Interação do Usuário:
     - Clique duplo em uma célula dispara `cxDBVtableCellDblClick`.
     - Alteração de valor dispara `cxDBVtableEditValueChanged`.
     - Mudança de registro dispara `cxDBVtableFocusedRecordChanged`.

* **Dados Necessários:**
  - Código do Mill.
  - Código do Carrier.
  - Código SAP (opcional).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se um registro estiver selecionado.
  - **Salvar:** Disponível apenas se houver alterações válidas.

* **Filtros Disponíveis:**
  - Filtros por código ou nome.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Código SAP inválido" se o código SAP não for válido.

* **Valores Padrão dos Campos:**
  - `stat`: "Ativo".
  - `millDtime`: Data e hora atual.

* **Validações e Condições dos Campos:**
  - `sapCode`: Deve ser único e seguir um formato específico.
  - `mill`: Obrigatório.
  - `millCarrier`: Obrigatório.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_validate`: Valida os dados antes de salvar.
  - `m_CheckNewSapCode`: Verifica se o código SAP é único.
  - `m_SaveSapCodes`: Salva os códigos SAP no banco de dados.
  - `m_FindByCodeMill`: Pesquisa registros por código.
  - `m_ChangeEditableFields`: Altera os campos editáveis com base no estado atual.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - **Nome do Serviço:** MillWithoutDuplicServiceUtils.
  - **Endpoint:** Não especificado no código.
  - **Dados Enviados:** Não especificado no código.
  - **Dados Recebidos:** Não especificado no código.
  - **Propósito:** Verificar duplicidade de registros.
  - **Tratamento de Erros:** Mensagem de erro exibida em caso de falha.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o frame.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `mill` (string, obrigatório).
  - `millCarrier` (string, obrigatório).
  - `stat` (string, opcional).
  - `millDtime` (datetime, opcional).
  - `sapCode` (string, opcional, único).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `mill` → Coluna `mill`.
  - `millCarrier` → Coluna `carrier`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEcarrierMill := TFRAMEcarrierMill.Create(Self);
  FRAMEcarrierMill.HasSapCode := MyEventHandler;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>Mill</th>
        <th>Mill Carrier</th>
        <th>Status</th>
        <th>Mill Dtime</th>
        <th>SAP Code</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Mill1</td>
        <td>Carrier1</td>
        <td>Ativo</td>
        <td>2023-10-01 10:00</td>
        <td>SAP123</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial da grade:
  ```delphi
  MasterKeyFields := 'code=carrier';
  DataPacketName := 'LnkCarrier';
  ```

* Definição de ações disponíveis:
  ```delphi
  AvailableActions := 'ADD;DELETE';
  ```

---

## 12. Conclusão:

O código `FRcarrierMill` é uma implementação robusta para gerenciar dados em uma interface de grade. Ele oferece funcionalidades essenciais como validação, edição e salvamento de dados. No entanto, faltam detalhes sobre endpoints de serviços externos e mensagens de erro específicas.

---

## 13. Resumo Curto:

O `FRcarrierMill` é um frame Delphi para gerenciar dados de "Mills" e "Carriers" em uma grade interativa, com suporte a validação, edição e integração com serviços SOAP.#### **FRcarrierMill.pas**

```
unit FRcarrierMill;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneUtils;

type
  TFRAMEcarrierMill = class(TFRAMEBaseGridEditSOA)
    procedure CDStableAfterScroll(DataSet: TDataSet);
    procedure cxDBVtableCellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure cxDBVtableFocusedItemChanged(Sender: TcxCustomGridTableView;
      APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem);
    procedure cxDBVtableFocusedRecordChanged(
      Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord;
      ANewItemRecordFocusingChanged: Boolean);
    procedure cxDBVtableStylesGetContentStyle(
      Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; out AStyle: TcxStyle);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    FSapCodes: TStringList;
    FHasSapCode: TNotifyWithBooleanEvent;
    FOpenSapCodeForm: TNotifyEvent;
    procedure SetHasSapCode(const Value: TNotifyWithBooleanEvent);
    procedure SetOpenSapCodeForm(const Value: TNotifyEvent);
    procedure m_FindByCodeMill(Sender: TcxCustomGridTableView;
        AItem: TcxCustomGridTableItem);
    procedure m_FindMill(Sender: TObject; AButtonIndex: Integer);
    procedure SetKeyEditing(const EditKey: Boolean); override;
    procedure m_ChangeEditableFields;
    procedure m_SaveSapCodes;
    procedure m_GetSelectedMills;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    property HasSapCode: TNotifyWithBooleanEvent read FHasSapCode write SetHasSapCode;
    property OpenSapCodeForm: TNotifyEvent read FOpenSapCodeForm write SetOpenSapCodeForm;
    function m_validate: Boolean;  override;
    function m_CheckNewSapCode(const pv_NewSapCode: String): Boolean;
    destructor Destroy; override;
  end;

var
  FRAMEcarrierMill: TFRAMEcarrierMill;
  gv_AddedMills: String;

implementation

uses
  kneTypes, kneFREditSOA, kneFindDialog{#NAVOPTECH2022-785}, Global{#NAVOPTECH2022-875}
  ,kneDialogFactory{#NAVOPTECH2022-785}, kneFGFindUtils{#NAVOPTECH2022-785}
  , MillWithoutDuplicServiceUtils{#NAVOPTECH2022-785};

{$R *.dfm}

{ TFRAMEcarrierMill }

constructor TFRAMEcarrierMill.Create(AOwner: TComponent);
begin
	inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'code=carrier';
  DataPacketName := 'LnkCarrier';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'links';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');

    // Ordem Campos ............................................................
    DefineReadOnlyFields('mill;millCarrier;stat;millDtime;sapCode');

    // Key Fields ..............................................................
    KeyFields:= 'mill;carrier';

    //NAVOPTECH2022-785 (cmosilva 26-05-2022)
    // Custom Editors ..........................................................
```

#### **FRcarrierMill.dfm**

```
inherited FRAMEcarrierMill: TFRAMEcarrierMill
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnCellDblClick = cxDBVtableCellDblClick
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
end
```
<!-- tabs:end -->

